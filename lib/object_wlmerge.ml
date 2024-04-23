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
    | Rec of { concrete : concrete_table; symbolic : symb_slot; time : int }
    | If of { cond : pc_value; then_ : t; else_ : t; time : int }

  and t = record list

  let time = ref 0

  let get_new_time () =
    time := !time + 1;
    !time

  let create_record ?(time = 0) () : record =
    Rec { concrete = Hashtbl.create 16; symbolic = None; time }

  let create_if_record ?(time = 0) (cond : pc_value) (then_ : t) (else_ : t) :
    record =
    If { cond; then_; else_; time }

  let create () : t = [ create_record () ]

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
      | Rec { concrete; symbolic; time } ->
        fprintf fmt "{%a{%a}, %a}" pp_int time pp_concrete concrete pp_symbolic
          symbolic
      | If { cond; then_; else_; time } ->
        fprintf fmt "[ %a{%a then %a else %a} ]" pp_int time Expr.pp cond
          (pp_lst ~pp_sep:pp_semicolon pp_record)
          then_
          (pp_lst ~pp_sep:pp_semicolon pp_record)
          else_
    in
    fprintf fmt "[ %a ]"
      (pp_lst ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_record)
      o

  let to_string (o : t) : string = Fmt.asprintf "%a" pp o
  let to_json = to_string

  let clone (o : t) : t =
    let new_rec = create_record ~time:(get_new_time ()) () in
    new_rec :: o

  let merge (o1 : t) (o2 : t) (pc : pc_value) : t =
    let open Utils.List in
    let rec get_common_time (obj1 : t) (obj2 : t) : int =
      let exists_time time' =
        List.exists (fun x ->
            match x with Rec { time; _ } | If { time; _ } -> time = time' )
      in
      match obj1 with
      | [] -> failwith "object_wlmerge.merge: unexpected case"
      | [ (Rec { time; _ } | If { time; _ }) ] ->
        if exists_time time obj2 then time
        else failwith "object_wlmerge.merge: unexpected case"
      | (Rec { time; _ } | If { time; _ }) :: tl ->
        if exists_time time obj2 then time else get_common_time tl obj2
    in
    let diff_times time' = function
      | Rec { time; _ } | If { time; _ } -> time <> time'
    in

    let time = get_common_time o1 o2 in
    let rest_o1, shared_obj = split_while o1 ~f:(diff_times time) in
    let rest_o2, _ = split_while o2 ~f:(diff_times time) in
    let if_record =
      create_if_record ~time:(get_new_time ()) pc rest_o1 rest_o2
    in
    let empty_record = create_record ~time:(get_new_time ()) () in
    empty_record :: if_record :: shared_obj

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
        let empty_record = create_record ~time:cur.time () in
        [ (empty_record :: new_record :: tl, pc) ] )
    | _ -> failwith "Object_wlmerge.set: unexpected case"

  let get_concrete (concrete : concrete_table) (pc : pc_value) (p : value) :
    pc_value option * (pc_value * value option) list =
    let find_opt = Hashtbl.find_opt concrete in
    let find = Hashtbl.find concrete in

    match Expr.view p with
    | Val (Str s) -> (
      let v = find_opt s in
      match v with
      | Some v' -> (None, [ (true_, v') ])
      | _ -> (Some true_, []) )
    | _ -> (
      let keys = Hashtbl.keys concrete in
      let keys' = List.filter (fun k -> is_sat [ eq p (str k); pc ]) keys in
      match keys' with
      | [ k ] ->
        if pc => eq p (str k) then (None, [ (true_, find k) ])
        else (Some (ne p (str k)), [ (eq p (str k), find k) ])
      | _ ->
        ( Some
            (List.fold_right
               (fun k acc -> and_ acc (ne p (str k)))
               keys' true_ )
        , List.map (fun k -> (eq p (str k), find k)) keys' ) )

  let get_record (r : record) (pc : pc_value) (p : value) :
    pc_value option * (pc_value * value option) list =
    let open Utils.Option in
    match r with
    | Rec { concrete; symbolic; _ } -> (
      let get_concrete = get_concrete concrete in

      match symbolic with
      | Some (p', v) ->
        if pc => eq p p' then (None, [ (true_, v) ])
        else if is_sat [ pc; eq p p' ] then
          let explore_pc, pvs = get_concrete (and_ pc (ne p p')) p in
          let explore_pc' =
            map_default (fun pc -> Some (and_ pc (ne p p'))) None explore_pc
          in
          (explore_pc', (eq p p', v) :: pvs)
        else get_concrete pc p
      | None -> get_concrete pc p )
    | _ -> failwith "object_wlmerge.get_record: unexpected case"

  let rec get_if_record (r : record) (explored_pc : pc_value) (find_any : bool)
    (pc : pc_value) (p : value) =
    let add_cond cond = List.map (fun (pc, v) -> (and_ pc cond, v)) in
    let get_if o cond =
      if is_sat [ pc; cond ] then
        get_aux p (and_ pc cond) (find_any, Some explored_pc, []) o
      else (true, None, [])
    in

    match r with
    | If { cond; then_; else_; _ } ->
      let then_b, then_pc, then_pvs = get_if then_ cond in
      let else_b, else_pc, else_pvs = get_if else_ (not_ cond) in

      let then_pvs, else_pvs =
        (add_cond cond then_pvs, add_cond (not_ cond) else_pvs)
      in
      let pvs' = then_pvs @ else_pvs in

      let r' =
        match (then_b, then_pc, else_b, else_pc) with
        | true, None, true, None -> None
        | false, None, false, None -> Some (true_)
        | true, None, false, None -> Some (not_ cond)
        | false, None, true, None -> Some cond
        | true, None, false, Some r -> Some (and_ r (not_ cond))
        | false, Some r, true, None -> Some (and_ r cond)
        | false, None, false, Some r -> Some (or_ cond (and_ r (not_ cond)))
        | false, Some r, false, None -> Some (or_ (not_ cond) (and_ r cond))
        | false, Some r1, false, Some r2 ->
          Some (or_ (and_ r1 cond) (and_ r2 (not_ cond)))
        | _ ->
          Format.printf "(%b, %a, %b, %a)\n" then_b (Fmt.pp_opt Expr.pp) then_pc
            else_b (Fmt.pp_opt Expr.pp) else_pc;
          failwith "object_wlmerge.get_if_record: unexpected case"
      in
      (r', pvs')
    | _ -> failwith "object_wlmerge.get_if_record: unexpected case"

  and
    (* returns (b, explored_pc, pvs)
       b:
        True = stop searching;
        False = continue searching
       explore_pc:
        None = Nothing to consider/stop searching;
        Some pc = consider pc during searching
       pvs:
        a list of condition with the respective value
    *)
    get_aux (p : value) (pc : pc_value)
    ((r, explored_pc, pvs) :
      bool * pc_value option * (pc_value * value option) list ) (o : t) :
    bool * pc_value option * (pc_value * value option) list =
    (* TODO: pc_value option to pc_value *)
    let explored_pc = Option.get explored_pc in

    match o with
    | [] -> (r, Some explored_pc, pvs)
    | (Rec _ as cur) :: parent -> (
      let explored_pc', pvs' = get_record cur explored_pc p in

      let pvs'' = pvs @ pvs' in
      match explored_pc' with
      | None -> (true, None, pvs'')
      | Some b -> get_aux p pc (r, Some (and_ explored_pc b), pvs'') parent )
    | (If _ as cur) :: parent -> (
      let explored_pc', pvs' = get_if_record cur explored_pc r pc p in

      let pvs'' = pvs @ pvs' in
      match explored_pc' with
      | None -> (true, None, pvs'')
      | Some b -> get_aux p pc (r, Some (and_ explored_pc b), pvs'') parent )

  let mk_ite_get (conds : (pc_value * value option) list) : value =
    if List.exists (fun (_, v) -> Option.is_some v) conds then
      List.fold_right
        (fun (cond, v) acc -> ite cond (Option.value v ~default:undef) acc)
        conds undef
    else undef

  let get (o : t) (field : value) (pc : pc_value) : (value * pc_value) list =
    let _, _, l = get_aux field pc (false, Some pc, []) o in
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
        let empty_record = create_record ~time:cur.time () in
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
          let v' = map_default (fun _ -> true_) (false_) v in
          ite cond v' acc )
        conds (false_)
    else false_

  let has_field (o : t) (field : value) (pc : pc_value) : value =
    let _, _, l = get_aux field pc (false, Some pc, []) o in
    let ite_expr = mk_ite_has_field l in
    ite_expr
end
