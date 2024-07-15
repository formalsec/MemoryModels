open Utils.Option
open Utils.Encoding
open Smtml

(* MWL Model that record type (cur; Some parent) *)
module M :
  Object_intf.S with type value = Smtml.Expr.t and type pc_value = Smtml.Expr.t =
struct
  type value = Smtml.Expr.t
  type pc_value = Smtml.Expr.t
  type symb_slot = (value * value option) option
  type concrete_table = (string, value option) Hashtbl.t
  type record = { concrete : concrete_table; symbolic : symb_slot }
  type t = { cur : record; parent : t option }

  let create_record () : record =
    { concrete = Hashtbl.create 16; symbolic = None }

  let create () : t = { cur = create_record (); parent = None }

  let clone (o : t) : t =
    let new_rec = create_record () in
    { cur = new_rec; parent = Some o }

  let merge (_o1 : t) (_o2 : t) (_pc : pc_value) : t =
    failwith "Object_mwl.merge not implemented"

  let set (o : t) ~(field : value) ~(data : value) (pc : pc_value) :
    (t * pc_value) list =
    match Expr.view field with
    | Val (Str field) ->
      Hashtbl.replace o.cur.concrete field (Some data);
      [ (o, pc) ]
    | _ ->
      let new_record = { o.cur with symbolic = Some (field, Some data) } in
      let new_o = { o with cur = new_record } in
      let empty_record = create_record () in
      [ ({ cur = empty_record; parent = Some new_o }, pc) ]

  let get_concrete (concrete : concrete_table) (pc : pc_value) (p : value) :
    pc_value option * (pc_value * value option) list =
    let find_opt = Hashtbl.find_opt concrete in
    let find = Hashtbl.find concrete in

    match Expr.view p with
    | Val (Str s) -> (
      let v = find_opt s in
      match v with Some v' -> (None, [ (true_, v') ]) | _ -> (Some pc, []) )
    | _ -> (
      let keys = Hashtbl.keys concrete in
      let keys' = List.filter (fun k -> is_sat [ eq p (str k); pc ]) keys in
      match keys' with
      | [ k ] ->
        if pc => eq p (str k) then (None, [ (true_, find k) ])
        else (Some (and_ pc (ne p (str k))), [ (eq p (str k), find k) ])
      | _ ->
        ( Some (List.fold_right (fun k acc -> and_ acc (ne p (str k))) keys' pc)
        , List.map (fun k -> (eq p (str k), find k)) keys' ) )

  let get_record ({ concrete; symbolic } : record) (pc : pc_value) (p : value) :
    pc_value option * (pc_value * value option) list =
    let get_concrete = get_concrete concrete in

    match symbolic with
    | Some (p', v) ->
      if pc => eq p p' then (None, [ (true_, v) ])
      else if is_sat [ pc; eq p p' ] then
        let b, pvs = get_concrete (and_ pc (ne p p')) p in
        (b, (eq p p', v) :: pvs)
      else get_concrete pc p
    | None -> get_concrete pc p

  let rec get_aux (p : value) (pc : pc_value)
    ((r, pvs) : pc_value option * (pc_value * value option) list)
    { cur; parent } : (pc_value * value option) list =
    let r =
      match r with
      | Some r -> r
      | None -> failwith "Object_mwl.get_aux unsupported case"
    in
    let r', pvs' = get_record cur r p in
    let pvs'' = pvs @ pvs' in
    match r' with
    | None -> pvs''
    | Some b -> map_default (get_aux p pc (Some (and_ r b), pvs'')) pvs'' parent

  let mk_ite_get (conds : (pc_value * value option) list) : value =
    if List.exists (fun (_, v) -> Option.is_some v) conds then
      List.fold_right
        (fun (cond, v) acc -> ite cond (Option.value v ~default:undef) acc)
        conds undef
    else undef

  let get (o : t) (field : value) (pc : pc_value) : (value * pc_value) list =
    let l = get_aux field pc (Some pc, []) o in
    let ite_expr = mk_ite_get l in
    [ (ite_expr, pc) ]

  let delete (o : t) (field : value) (pc : pc_value) : (t * pc_value) list =
    match Expr.view field with
    | Val (Str field) ->
      Hashtbl.replace o.cur.concrete field None;
      [ (o, pc) ]
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
    match o.parent with
    | Some _ -> assert false
    | None ->
      Hashtbl.fold
        (fun k v acc -> match v with Some _ -> str k :: acc | None -> acc)
        o.cur.concrete []

  let mk_ite_has_field (conds : (pc_value * value option) list) : value =
    if List.exists (fun (_, v) -> Option.is_some v) conds then
      List.fold_right
        (fun (cond, v) acc ->
          let v' = map_default (fun _ -> true_) false_ v in
          ite cond v' acc )
        conds false_
    else false_

  let has_field (o : t) (field : value) (pc : pc_value) : value =
    let l = get_aux field pc (Some pc, []) o in
    let ite_expr = mk_ite_has_field l in
    ite_expr

  let rec pp (fmt : Fmt.t) (o : t) : unit =
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
    let pp_record fmt { concrete; symbolic } =
      fprintf fmt "{{%a}, %a}" pp_concrete concrete pp_symbolic symbolic
    in
    let pp_parent fmt v = pp_opt (fun fmt h -> fprintf fmt "%a" pp h) fmt v in
    fprintf fmt "%a ;@\n%a " pp_record o.cur pp_parent o.parent

  let to_string (o : t) : string = Fmt.asprintf "%a" pp o
  let to_json = to_string
end