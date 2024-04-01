open Utils.Encoding
open Encoding

module M :
  Object_intf.S
    with type value = Encoding.Expr.t
     and type pc_value = Encoding.Expr.t = struct
  type value = Encoding.Expr.t
  type pc_value = Encoding.Expr.t
  type symb_slot = (value * value option) option
  type concrete_table = (string, value option) Hashtbl.t

  type record =
    { concrete : concrete_table
    ; symbolic : symb_slot
    }

  type t =
    { cur : record
    ; parent : t option
    }

  let create_record () : record =
    { concrete = Hashtbl.create 16; symbolic = None }

  let create () : t = { cur = create_record (); parent = None }

  let clone (o : t) : t =
    let new_rec = create_record () in
    { cur = new_rec; parent = Some o }

  let set (o : t) ~(field : value) ~(data : value) (pc : pc_value) :
    (t * pc_value) list =
    match Expr.view field with
    (* Concrete *)
    | Val (Str field) ->
      Hashtbl.replace o.cur.concrete field (Some data);
      [ (o, pc) ]
    (* Symbolic *)
    | _ ->
      let new_record = { o.cur with symbolic = Some (field, Some data) } in
      let new_o = { o with cur = new_record } in
      let empty_record = create_record () in
      [ ({ cur = empty_record; parent = Some new_o }, pc) ]

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
     Notes:
        - It returns a type of (... * value option option)
        because the outter option is to know if there is any direct binding in the record
         and the inner option is know if ot was deleted or not and that with this we can apply [get_val]

      Possible problems/TODO:
      - after deleting a field, does this function return the correct value?
      - what happen when there is several records with the same field? will it, should it return everything?
        - maybe it is solved with the final value that is passed
  *)
  let get_aux_rec (r : record) (field : value) (pc : pc_value)
    (get_val : value option -> value) :
    (value * value) list * value option option =
    let lst, v =
      match r.symbolic with
      | None -> ([], None)
      | Some (f, v) when Expr.equal field f -> ([], Some v)
      | Some (f, v) ->
        let cond = eq field f in
        let new_pc = Expr.(Bool.and_ pc cond) in
        if is_sat [ new_pc ] then ([ (get_val v, cond) ], None) else ([], None)
    in
    match v with
    | Some _ -> (lst, v)
    | None ->
      let lst, v =
        match Expr.view field with
        | Val (Str f) -> (
          match Hashtbl.find_opt r.concrete f with
          | Some v -> (lst, Some v)
          | _ -> (lst, v) )
        | _ ->
          ( Hashtbl.fold
              (fun f v acc ->
                let cond = eq field (str f) in
                let new_pc = Expr.(Bool.and_ pc cond) in
                if is_sat [ new_pc ] then (get_val v, cond) :: acc else acc )
              r.concrete lst
          , v )
      in
      (lst, v)

  (* Auxiliar function: [_get_prop o field pc acc get_val] transverse the object to get every possible value of the given field.
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

  let rec _get_prop ?(default_val : value = undef) (o : t option) (field : value)
    (pc : pc_value) (acc : (value * value) list)
    (get_val : value option -> value) : (value * value) list * value =
    match o with
    | Some { cur; parent } -> (
      let lst, final_val = get_aux_rec cur field pc get_val in
      match (lst, final_val) with
      (* Only found the final value *)
      | [], Some v ->
        (* Format.printf "([], Some e)\n"; *)
        (acc, get_val v)
      (* A list with some condition values and a final value *)
      | l, Some v ->
        (* Format.printf "(l, Some e)\n"; *)
        (l @ acc, get_val v)
      (* Found nothing in this record *)
      | [], None ->
        (* Format.printf "([],  None)\n"; *)
        _get_prop ~default_val parent field pc acc get_val
      (* Field is symbolic and there is nothing in the record *)
      | ((v, _) :: tl as l), None ->
        let new_pc =
          neg
            (List.fold_left
               (fun acc (_, c) ->
                 if Expr.equal (boolean false) acc then c
                 else Expr.Bool.or_ acc c )
               (boolean false) l )
        in
        if is_sat [ new_pc; pc ] then
          let new_pc = Expr.Bool.and_ pc new_pc in
          _get_prop ~default_val parent field new_pc (l @ acc) get_val
        else (tl @ acc, v) )
    | None -> (acc, default_val)

  let mk_ite (conds : (pc_value * value option) list) : value =
    List.fold_right
      (fun (cond, v) acc -> match v with None -> acc | Some _ -> ite cond (boolean true) acc)
      conds (boolean false)

  let make_ite (conds : (pc_value * value option) list) : value =
    List.fold_right
      (fun (cond, v) acc -> match v with None -> acc | Some v -> ite cond v acc)
      conds undef
  
  let get_concrete (concrete : concrete_table) (pc: pc_value) (p: value) : (pc_value option * (pc_value * value option) list )=
    let find_opt = Hashtbl.find_opt concrete in
    let find = Hashtbl.find concrete in

    match Expr.view p with
    | Val (Str s) -> 
      (let v = find_opt s in
      match v with
      | Some v' -> (None, [boolean true, v'])
      | _ -> (Some (boolean true), []))
    | _ ->
      let keys = Hashtbl.keys concrete in
      let keys' = List.filter (fun k -> is_sat [eq p (str k); pc]) keys in
      match keys' with
      | [ k ] -> (Some (ne p (str k)), [(eq p (str k)), find k])
      | _ -> (Some (List.fold_right (fun k acc -> Expr.Bool.and_ acc (ne p (str k))) keys' (boolean true) ), List.map (fun k -> (eq p (str k), find k)) keys')

    
    let get_record ({concrete; symbolic} : record) (pc : pc_value) (p : value) :  (pc_value option * (pc_value * value option) list )  =
      let get_concrete = get_concrete concrete pc in

      match symbolic with
      | Some (p', v) -> 
        (* symbolic exists *)
        (* FIXME: 
           se pc = true, p = banana, p = x => entra aqui neste if, mas não era suposto *)
        (* if (pc => (eq p p')) then  *)
        if Expr.equal p p' then 
          (None, [(boolean true, v)])
        else if (is_sat [pc; eq p p']) then
          let (b, pvs) = get_concrete p in
          let new_b = 
            match b with 
          | Some b -> Some (Expr.Bool.and_ b (ne p p')) 
          | None -> Some (ne p p')
          in
          (new_b, (eq p p', v) :: pvs)
        else
          get_concrete p
        | None -> get_concrete p


    let rec get_aux (p : value) (pc:pc_value) ((r, pvs) :  pc_value option * (pc_value * value option) list) {cur ; parent} : (pc_value * value option) list  =
      let open Utils.Option in
      let r = match r with Some r -> r | None -> boolean true in
      let (r', pvs') = get_record cur (Expr.Bool.and_ pc r) p in 
      let pvs'' = pvs @ pvs' in
      match r' with 
      | None -> pvs''
      | Some b -> map_default (get_aux p pc (Some (Expr.Bool.and_ r b),pvs'')) pvs'' parent

    let get (o : t) (field : value) (pc : pc_value) : (value * pc_value) list =
      let l = get_aux field pc (None, []) o in 
      let ite_expr = make_ite l in 
      (* Format.printf "\n----------\nget %a: %a\n\n ite: %a----------\n \n" 
      Expr.pp field 
      (Fmt.pp_lst ~pp_sep:Fmt.pp_semicolon (fun fmt (pc, v) -> Fmt.fprintf fmt "(%a, %a)" Expr.pp pc  Expr.pp (Option.value v ~default:(undef)))) l
      Expr.pp ite_expr; *)
      [(ite_expr, pc)]

  let delete (o : t) (field : value) (pc : pc_value) : (t * pc_value) list =
    match Expr.view field with
    (* Concrete *)
    | Val (Str field) ->
      Hashtbl.replace o.cur.concrete field None;
      [ (o, pc) ]
    (* Symbolic *)
    | _ ->
      let new_record = { o.cur with symbolic = Some (field, None) } in
      let new_o = { o with cur = new_record } in
      let empty_record = create_record () in
      [ ({ cur = empty_record; parent = Some new_o }, pc) ]

  let to_list (o : t) : (value * value) list =
    (* Just calculates if there are just concrete fields *)
    match o.parent with
    | Some _ -> assert false
    | None ->
      Hashtbl.fold
        (fun k v acc -> match v with Some v -> (str k, v) :: acc | None -> acc)
        o.cur.concrete []

  let get_fields (o : t) : value list =
    (* Just calculates if there are just concrete fields *)
    let rec aux { cur; parent } acc =
      let concrete_list =
        Hashtbl.fold
          (fun k v acc -> match v with Some _ -> str k :: acc | None -> acc)
          cur.concrete acc
      in
      let symbolic_list =
        match cur.symbolic with
        | None -> []
        | Some (k, v) -> if Option.is_none v then [] else [ k ]
      in
      match parent with
      | None -> symbolic_list @ concrete_list
      | Some p -> aux p (symbolic_list @ concrete_list)
    in
    aux o []

  let has_field (o : t) (field : value) (pc : pc_value) : value =
    let l = get_aux field pc (None, []) o in 
    let ite_expr = mk_ite l in
    (* Format.printf "\n----------\nhas_field %a: %a\n\n ite: %a----------\n \n" 
      Expr.pp field 
      (Fmt.pp_lst ~pp_sep:Fmt.pp_semicolon (fun fmt (pc, v) -> Fmt.fprintf fmt "(%a, %a)" Expr.pp pc  Expr.pp (Option.value v ~default:(undef)))) l
      Expr.pp ite_expr; *)
    (* Format.printf "Has_field %a : %a\n" Expr.pp field Expr.pp ite_expr; *)
    ite_expr

  let rec pp (fmt : Fmt.t) ({ cur; parent } : t) : unit =
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
    fprintf fmt "%a{@\n- Concrete table:@\n{%a}@\n- Symbolic slot: %a }"
      pp_parent parent pp_concrete cur.concrete pp_symbolic cur.symbolic

  let to_string (o : t) : string = Fmt.asprintf "%a" pp o
  let to_json = to_string
end
