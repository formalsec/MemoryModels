open Utils.Encoding
open Encoding

type value = Encoding.Expr.t
type symb_slot = (value * value option) option
type concrete_table = (string, value option) Hashtbl.t
type record = { concrete : concrete_table; symbolic : symb_slot }
type t = { record : record; parent : t option }

let create_record () : record =
  { concrete = Hashtbl.create 16; symbolic = None }

let create () : t = { record = create_record (); parent = None }

let clone (o : t) : t =
  let new_rec = create_record () in
  { record = new_rec; parent = Some o }

let is_empty (o : t) : bool =
  (* FIXME: when there is several instances of fields that was deleted *)
  let symb_empty s = match s with Some (_, Some _) -> false | _ -> true in
  let concrete_empty c =
    Hashtbl.fold
      (fun _ v acc -> match v with Some _ -> false | None -> acc)
      c true
  in
  let rec aux { record; parent } : bool =
    match parent with
    | None -> symb_empty record.symbolic && concrete_empty record.concrete
    | Some p ->
      symb_empty record.symbolic && concrete_empty record.concrete && aux p
  in
  aux o

let set (o : t) ~(field : value) ~(data : value) (pc : value) : (t * value) list
    =
  match Expr.view field with
  (* Concrete *)
  | Val (Str field) ->
    Hashtbl.replace o.record.concrete field (Some data);
    [ (o, pc) ]
  (* Symbolic *)
  | _ ->
    let new_record = { o.record with symbolic = Some (field, Some data) } in
    let new_o = { o with record = new_record } in
    let empty_record = create_record () in
    [ ({ record = empty_record; parent = Some new_o }, pc) ]

(* Auxiliar function: [get_aux_rec r field pc get_val] gets all possible values from a record.
    It returns a list of (value, condition) and a final value (the direct binding if encounter in this record).
    The function get_val is used to switch between [has_field] and [get].
   Idea:
   1. Search in the symbolic slot
     1.1 if found a direct binding -> return
     1.2 if found a binding -> add the value and condition to the list if SAT
   2. Search in the concrete table
     2.1 concrete field - search directly in the concrete table
     2.2 symbolic field - add the value and condition of all binding to the list if SAT


   Possible problems/TODO:
   - after deleting a field, does this function return the correct value?
   - what happen when there is severeal records with the same field? will it, should it return everything?
     - maybe it is already solved with the last match of get_prop
*)
let get_aux_rec (r : record) (field : value) (pc : value)
  (get_val : value option -> value) :
  (value * value) list * (value option * value option) option =
  let lst, v =
    match r.symbolic with
    | None -> ([], None)
    | Some (f, v) when Expr.equal field f -> ([], Some (v, None))
    | Some (f, v) ->
      let new_pc = eq field f in
      if is_sat [ new_pc; pc ] then ([ (get_val v, new_pc) ], None)
      else ([], None)
  in
  let lst, v =
    match Expr.view field with
    | Val (Str f) -> (
      match Hashtbl.find_opt r.concrete f with
      | Some v -> (lst, Some (v, None))
      | _ -> (lst, v) )
    | _ -> (
      match v with
      | Some _ -> (lst, v)
      | None ->
        ( Hashtbl.fold
            (fun f v acc ->
              let new_pc = eq field (str f) in
              if is_sat [ new_pc; pc ] then (get_val v, new_pc) :: acc else acc
              )
            r.concrete lst
        , v ) )
  in
  (lst, v)

(* Auxiliar function: [get_prop o field pc acc get_val] transverse the object to get every possible value of the given field.
    It returns a list of (value, condition) and a final value (the direct binding if encounter in this record).
   Idea: (For every record)
   1. get the values form the record (applying get_aux_rec)
   2. Have a direct binding in the record -> return
   3. No direct binding in the record
     3.1 and there is no value founded -> search in the parent
     3.2 found values ->
       if cond=SAT add to list and continue to serach
       else return (TODO: why do we add the tl to the acc?)
*)

let rec get_prop ?(default_val : value = undef) (o : t option) (field : value)
  (pc : value) (acc : (value * value) list list)
  (get_val : value option -> value) :
  (value * value) list list * (value * value option) =
  match o with
  | Some { record; parent } -> (
    let lst, final_val = get_aux_rec record field pc get_val in
    match (lst, final_val) with
    (* Only found the final value *)
    | [], Some (v, pc') ->
      Format.printf "([], Some e)\n";
      (acc, (get_val v, pc'))
    (* A list with some condition values and a final value *)
    | l, Some (v, pc') ->
      Format.printf "(l, Some e)\n";
      (l :: acc, (get_val v, pc'))
    (* Found nothing in this record *)
    | [], None ->
      Format.printf "([],  None)\n";
      get_prop ~default_val parent field pc acc get_val
    (* Field is symbolic and there is nothing in the record *)
    | ((v, cond) :: tl as l), None ->
      let new_pc =
        neg
          (List.fold_left
             (fun acc (_, c) ->
               if Expr.equal (boolean false) acc then c else Expr.Bool.or_ acc c
               )
             (boolean false) l )
      in
      if is_sat [ new_pc; pc ] then
        let new_pc = Expr.Bool.and_ pc new_pc in
        get_prop ~default_val parent field new_pc (l :: acc) get_val
      else (tl :: acc, (v, Some cond)) )
  | None -> (acc, (default_val, None))

let mk_ite (conds : (value * value) list list) (final_val : value) : value =
  List.fold_left
    (fun acc_ite l ->
      List.fold_left (fun acc (data, cond) -> ite cond data acc) acc_ite l )
    final_val conds

let rec mk_ite_aux accs acc_pc default_val default_pc conds =
  let ite, new_pc, new_default =
    List.fold_left
      (fun acc_ite l ->
        List.fold_left
          (fun (acc_val, acc_pc, new_default) (cond, data) ->
            if Expr.(Ty.equal (ty default_val) (ty data)) then
              let p =
                if Expr.equal (boolean false) acc_pc then cond
                else Expr.Bool.or_ acc_pc cond
              in
              let ite = ite cond data acc_val in
              (ite, p, new_default)
            else
              ( acc_val
              , acc_pc
              , if Expr.equal undef new_default then data else new_default ) )
          acc_ite l )
      (default_val, default_pc, undef)
      conds
  in

  if Expr.equal undef new_default then
    ((ite, Some new_pc) :: accs, Expr.Bool.or_ acc_pc new_pc)
  else
    mk_ite_aux
      ((ite, Some new_pc) :: accs)
      (Expr.Bool.or_ acc_pc new_pc)
      new_default (boolean false) conds

let mk_ite1 (conds : (value * value) list list)
  ((default_val, default_pc) : value * value option) :
  (value * value option) list =
  if List.length conds = 0 then [ (default_val, None) ]
  else
    let default_val, conds, default_pc, undef_possible =
      if Expr.equal default_val undef then
        let lst = List.hd conds in
        let data, cond = List.hd lst in
        (data, List.tl conds, cond, true)
      else
        let default_pc' =
          match default_pc with Some p -> p | None -> boolean false
        in
        (default_val, conds, default_pc', false)
    in

    let l, new_pc =
      mk_ite_aux [] (boolean false) default_val default_pc conds
    in

    let not_new = neg new_pc in

    if undef_possible then (undef, Some not_new) :: l else l

let get (o : t) (field : value) (pc : value) =
  Format.printf "\n-----------\n Get: %a\n" Expr.pp field;
  let get_v v = match v with None -> undef | Some v -> v in
  let l, v = get_prop (Some o) field pc [] get_v in

  let ite_expr = mk_ite1 l v in

  Format.printf "#%a#\n"
    (Fmt.pp_lst ~pp_sep:Fmt.pp_semicolon (fun fmt (v, pc) ->
         Format.fprintf fmt "%a, %a" Expr.pp v (Fmt.pp_opt Expr.pp) pc ) )
    ite_expr;
  ite_expr

let delete (o : t) (field : value) (pc : value) : (t * value) list =
  match Expr.view field with
  (* Concrete *)
  | Val (Str field) ->
    Hashtbl.replace o.record.concrete field None;
    [ (o, pc) ]
  (* Symbolic *)
  | _ ->
    let new_record = { o.record with symbolic = Some (field, None) } in
    let new_o = { o with record = new_record } in
    let empty_record = create_record () in
    [ ({ record = empty_record; parent = Some new_o }, pc) ]

let to_list (o : t) : (value * value) list =
  (* FIXME: Does not consider when there repeated fields in the current record and in the parent record.
     What should it do? I think that it supposed to just output one for key*)
  let rec aux { record; parent } acc =
    let concrete_list =
      Hashtbl.fold
        (fun k v acc -> match v with Some v -> (str k, v) :: acc | None -> acc)
        record.concrete acc
    in
    let symbolic_list =
      match record.symbolic with
      | None -> []
      | Some (k, v) -> if Option.is_none v then [] else [ (k, Option.get v) ]
    in
    match parent with
    | None -> symbolic_list @ concrete_list
    | Some p -> aux p (symbolic_list @ concrete_list)
  in
  aux o []

let get_fields (o : t) : value list =
  (* FIXME: Does not consider when there repeated fields in the current record and in the parent record. *)
  let rec aux { record; parent } acc =
    let concrete_list =
      Hashtbl.fold
        (fun k v acc -> match v with Some _ -> str k :: acc | None -> acc)
        record.concrete acc
    in
    let symbolic_list =
      match record.symbolic with
      | None -> []
      | Some (k, v) -> if Option.is_none v then [] else [ k ]
    in
    match parent with
    | None -> symbolic_list @ concrete_list
    | Some p -> aux p (symbolic_list @ concrete_list)
  in
  aux o []

let has_field (o : t) (field : value) (pc : value) : value =
  let get_v v = match v with None -> boolean false | _ -> boolean true in
  let l, (v, _) =
    get_prop ~default_val:(boolean false) (Some o) field pc [] get_v
  in
  let ite_expr = mk_ite l v in
  Format.printf "Has_field %a : %a\n" Expr.pp field Expr.pp ite_expr;
  ite_expr

let rec pp (fmt : Fmt.t) ({ record; parent } : t) : unit =
  let open Fmt in
  let pp_v fmt (field, data) =
    fprintf fmt "%a: %a" pp_str field (pp_opt Expr.pp) data
  in
  let pp_concrete fmt tbl = pp_hashtbl ~pp_sep:pp_semicolon pp_v fmt tbl in
  let pp_symbolic fmt = function
    | None -> pp_str fmt "None"
    | Some (field, data) ->
      fprintf fmt "%a: %a" Expr.pp field (pp_opt Expr.pp) data
  in
  let pp_parent fmt v =
    pp_opt (fun fmt h -> fprintf fmt "%a@\n<-@\n" pp h) fmt v
  in
  fprintf fmt "%a{@\n- Concrete table:@\n{%a}@\n- Symbolic slot: %a }" pp_parent
    parent pp_concrete record.concrete pp_symbolic record.symbolic

let to_string (o : t) : string = Fmt.asprintf "%a" pp o
let to_json = to_string
