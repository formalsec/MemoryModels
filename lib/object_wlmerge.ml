open Utils.Encoding
open Encoding

module M :
  Object_intf.S
    with type value = Encoding.Expr.t
     and type pc_value = Encoding.Expr.t = struct
  type pc_value = Encoding.Expr.t
  type value = Encoding.Expr.t
  type symb_slot = (value * value option) option
  type concrete_table = (string, value option) Hashtbl.t

  type record =
    | Rec of { concrete : concrete_table; symbolic : symb_slot; id : int }
    | If of { cond : pc_value; then_ : t; else_ : t }

  and t = record list

  let id = ref 0

  let get_new_id () =
    id := !id + 1;
    !id

  let create_record ?(id = 0) () : record =
    Rec { concrete = Hashtbl.create 16; symbolic = None; id }

  let _create_if_record (cond : pc_value) (then_ : t) (else_ : t) : record =
    If { cond; then_; else_ }

  let create () : t = [ create_record () ]

  let clone (o : t) : t =
    let new_rec = create_record ~id:(get_new_id ()) () in
    new_rec :: o

  let merge (_o1 : t) (_o2 : t) (_pc : pc_value) : t =
    failwith "Object_mwl.merge not implemented"

  let set (o : t) ~(field : value) ~(data : value) (pc : pc_value) :
    (t * pc_value) list =
    match o with
    | Rec cur :: tl -> (
      match Expr.view field with
      | Val (Str field) ->
        Hashtbl.replace cur.concrete field (Some data);
        [ (o, pc) ]
      | _ ->
        let new_record = Rec { cur with symbolic = Some (field, Some data) } in
        let empty_record = create_record () in
        [ (empty_record :: new_record :: tl, pc) ] )
    | _ -> failwith "Object_wlmerge.set: unexpected case"

  let get_concrete (concrete : concrete_table) (pc : pc_value) (p : value) :
    pc_value option * (pc_value * value option) list =
    let find_opt = Hashtbl.find_opt concrete in
    let find = Hashtbl.find concrete in
    let bool_true = boolean true in

    match Expr.view p with
    | Val (Str s) -> (
      let v = find_opt s in
      match v with Some v' -> (None, [ (bool_true, v') ]) | _ -> (Some pc, []) )
    | _ -> (
      let keys = Hashtbl.keys concrete in
      let keys' = List.filter (fun k -> is_sat [ eq p (str k); pc ]) keys in
      match keys' with
      | [ k ] ->
        if pc => eq p (str k) then (None, [ (bool_true, find k) ])
        else (Some (and_ pc (ne p (str k))), [ (eq p (str k), find k) ])
      | _ ->
        ( Some (List.fold_right (fun k acc -> and_ acc (ne p (str k))) keys' pc)
        , List.map (fun k -> (eq p (str k), find k)) keys' ) )

  let get_record (r : record) (pc : pc_value) (p : value) :
    pc_value option * (pc_value * value option) list =
    match r with
    | Rec { concrete; symbolic; _ } -> (
      let get_concrete = get_concrete concrete in

      match symbolic with
      | Some (p', v) ->
        if pc => eq p p' then (None, [ (boolean true, v) ])
        else if is_sat [ pc; eq p p' ] then
          let b, pvs = get_concrete (and_ pc (ne p p')) p in
          (b, (eq p p', v) :: pvs)
        else get_concrete pc p
      | None -> get_concrete pc p )
    | _ -> failwith "object_wlmerge.get_record: unexpected case"

  let rec get_aux (p : value) (pc : pc_value)
    ((r, pvs) : pc_value option * (pc_value * value option) list) (o : t) :
    pc_value option * (pc_value * value option) list =
    let open Utils.List in
    let r = Option.get r in
    let add_cond lst cond = List.map (fun (pc, v) -> (and_ pc cond, v)) lst in

    match o with
    | [] -> (None, pvs)
    | (Rec _ as cur) :: parent -> (
      let r', pvs' = get_record cur r p in
      let pvs'' = pvs @ pvs' in
      match r' with
      | None -> (None, pvs'')
      | Some b ->
        map_default (get_aux p pc (Some (and_ r b), pvs'')) (None, pvs'') parent
      )
    | If { cond; then_; else_ } :: parent -> (
      let then_r, then_pvs = get_aux p (and_ pc cond) (Some r, []) then_ in
      let else_r, else_pvs =
        get_aux p (and_ pc (not_ cond)) (Some r, []) else_
      in
      let then_pvs, else_pvs =
        (add_cond then_pvs cond, add_cond else_pvs (not_ cond))
      in
      let pvs'' = then_pvs @ else_pvs in
      match (then_r, else_r) with
      (* Needs to revisit this cases *)
      | None, None ->
        map_default (get_aux p pc (Some r, pvs'')) (None, pvs'') parent
      | Some b, None | None, Some b ->
        map_default (get_aux p pc (Some (and_ r b), pvs'')) (None, pvs'') parent
      | Some b1, Some b2 ->
        map_default
          (get_aux p pc (Some (and_ r (or_ b1 b2)), pvs''))
          (None, pvs'') parent )

  let mk_ite_get (conds : (pc_value * value option) list) : value =
    if List.exists (fun (_, v) -> Option.is_some v) conds then
      List.fold_right
        (fun (cond, v) acc -> ite cond (Option.value v ~default:undef) acc)
        conds undef
    else undef

  let get (o : t) (field : value) (pc : pc_value) : (value * pc_value) list =
    let _, l = get_aux field pc (Some pc, []) o in
    let ite_expr = mk_ite_get l in
    [ (ite_expr, pc) ]

  let delete (o : t) (field : value) (pc : pc_value) : (t * pc_value) list =
    match o with
    | Rec cur :: tl -> (
      match Expr.view field with
      | Val (Str field) ->
        Hashtbl.replace cur.concrete field None;
        [ (o, pc) ]
      | _ ->
        let new_record = Rec { cur with symbolic = Some (field, None) } in
        let empty_record = create_record () in
        [ (empty_record :: new_record :: tl, pc) ] )
    | _ -> failwith "Object_wlmerge.delete: unexpected case"

  let to_list (o : t) : (value * value) list =
    (* Just calculates if there are just concrete fields *)
    match o with
    | [ Rec cur ] ->
      Hashtbl.fold
        (fun k v acc -> match v with Some v -> (str k, v) :: acc | None -> acc)
        cur.concrete []
    | _ -> assert false

  let get_fields (o : t) : value list =
    (* Just calculates if there are just concrete fields *)
    match o with
    | [ Rec cur ] ->
      Hashtbl.fold
        (fun k v acc -> match v with Some _ -> str k :: acc | None -> acc)
        cur.concrete []
    | _ -> assert false

  let mk_ite_has_field (conds : (pc_value * value option) list) : value =
    let open Utils.Option in
    if List.exists (fun (_, v) -> Option.is_some v) conds then
      List.fold_right
        (fun (cond, v) acc ->
          let v' = map_default (fun _ -> boolean true) (boolean false) v in
          ite cond v' acc )
        conds (boolean false)
    else boolean false

  let has_field (o : t) (field : value) (pc : pc_value) : value =
    let _, l = get_aux field pc (Some pc, []) o in
    let ite_expr = mk_ite_has_field l in
    ite_expr

  let pp (fmt : Fmt.t) (o : t) : unit =
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
    let rec pp_record fmt (r : record) =
      match r with
      | Rec { concrete; symbolic; _ } ->
        fprintf fmt "{{%a}, %a}" pp_concrete concrete pp_symbolic symbolic
      | If { cond; then_; else_ } ->
        fprintf fmt "[ %a then %a else %a ]" Expr.pp cond
          (pp_lst ~pp_sep:pp_semicolon pp_record)
          then_
          (pp_lst ~pp_sep:pp_semicolon pp_record)
          else_
    in
    fprintf fmt "[ %a ]@."
      (pp_lst ~pp_sep:(fun fmt () -> fprintf fmt ";@\n") pp_record)
      o

  let to_string (o : t) : string = Fmt.asprintf "%a" pp o
  let to_json = to_string
end
