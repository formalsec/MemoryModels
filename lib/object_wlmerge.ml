open Utils.Encoding
open Utils.Option
open Smtml

module M :
  Object_intf2.S with type value = Smtml.Expr.t and type pc_value = Smtml.Expr.t =
struct
  type pc_value = Smtml.Expr.t
  type value = Smtml.Expr.t
  type symb_slot = (value * value option) option
  type concrete_table = (string, value option) Hashtbl.t

  type record =
    | Rec of { concrete : concrete_table; symbolic : symb_slot; time : int }
    | If of { cond : pc_value; then_ : t; else_ : t; time : int }
    | Empty of int

  and t = record list

  let create_empty_record ?(time = 0) () : record = Empty time

  let create_record ?(time = 0) () : record =
    Rec { concrete = Hashtbl.create 16; symbolic = None; time }

  let create_if_record ?(time = 0) (cond : pc_value) (then_ : t) (else_ : t) :
    record =
    If { cond; then_; else_; time }

  let create ?(time = 0) () : t = [ create_empty_record ~time () ]

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
    let rec pp_record fmt (orec : record) =
      match orec with
      | Rec { concrete; symbolic; time } ->
        fprintf fmt "{ R%a{%a}, %a}" pp_int time pp_concrete concrete
          pp_symbolic symbolic
      | If { cond; then_; else_; time } ->
        fprintf fmt "[ R%a{%a then %a else %a} ]" pp_int time Expr.pp cond
          (pp_lst ~pp_sep:pp_semicolon pp_record)
          then_
          (pp_lst ~pp_sep:pp_semicolon pp_record)
          else_
      | Empty time -> fprintf fmt "{ R%a:Empty }" pp_int time
    in
    fprintf fmt "[ %a ]"
      (pp_lst ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_record)
      o

  let to_string (o : t) : string = Fmt.asprintf "%a" pp o

  let clone (o : t) (time : int) : t =
    let new_rec = create_empty_record ~time () in
    new_rec :: o

  let rec split (o_ac : t) (o : t) (time : int) : t * t =
    let get_time obj =
      match obj with Rec { time; _ } | If { time; _ } | Empty time -> time
    in

    match o with
    | [] -> (List.rev o_ac, [])
    | o_rec :: o' ->
      if get_time o_rec >= time then split (o_rec :: o_ac) o' time
      else (List.rev o_ac, o)

  let merge (o1 : t) (o2 : t) (time : int) (pc : pc_value) : t =
    let o1', o' = split [] o1 time in
    let o2', _ = split [] o2 time in
    let empty_record = create_empty_record ~time () in
    let if_record = create_if_record ~time pc o1' o2' in
    empty_record :: if_record :: o'

  let single_merge (o : t) (time : int) (pc : pc_value) : t =
    let o', o'' = split [] o time in
    let empty_record = create_empty_record ~time () in
    let if_record = create_if_record ~time pc o' [] in
    empty_record :: if_record :: o''

  let set (o : t) ~(field : value) ~(data : value) (pc : pc_value) :
    (t * pc_value) list =
    let o =
      match o with Empty t :: tl -> create_record ~time:t () :: tl | _ -> o
    in
    match o with
    | Rec cur :: tl -> (
      match Expr.view field with
      | Val (Str field) ->
        Hashtbl.replace cur.concrete field (Some data);
        [ (o, pc) ]
      | _ ->
        let new_record = Rec { cur with symbolic = Some (field, Some data) } in
        let empty_record = create_empty_record ~time:cur.time () in
        [ (empty_record :: new_record :: tl, pc) ] )
    | _ -> failwith "Object_wlmerge.set: unexpected case"

  let get_concrete (concrete : concrete_table) (pc : pc_value) (p : value) :
    bool * pc_value * (pc_value * value option) list =
    let find_opt = Hashtbl.find_opt concrete in
    let find = Hashtbl.find concrete in
    let all_different p keys =
      List.fold_right (fun k acc -> and_ acc (ne p (str k))) keys true_
    in

    match Expr.view p with
    | Val (Str s) -> (
      let v = find_opt s in
      match v with
      | Some v' -> (true, true_, [ (true_, v') ])
      | _ -> (false, true_, []) )
    | _ -> (
      let keys = Hashtbl.keys concrete in
      let keys' = List.filter (fun k -> is_sat [ eq p (str k); pc ]) keys in
      match keys' with
      | [ k ] ->
        if pc => eq p (str k) then (true, true_, [ (true_, find k) ])
        else (false, ne p (str k), [ (eq p (str k), find k) ])
      | _ ->
        ( false
        , all_different p keys'
        , List.map (fun k -> (eq p (str k), find k)) keys' ) )

  let rec get_record (orec : record) (pc : pc_value) (p : value) :
    bool * pc_value * (pc_value * value option) list =
    match orec with
    | Rec { concrete; symbolic; _ } -> (
      let get_concrete = get_concrete concrete in

      match symbolic with
      | Some (p', v) ->
        if pc => eq p p' then (true, true_, [ (true_, v) ])
        else if is_sat [ pc; eq p p' ] then
          let b, s_pc, pvs = get_concrete (and_ pc (ne p p')) p in
          let s_pc' = and_ s_pc (ne p p') in
          (b, s_pc', (eq p p', v) :: pvs)
        else get_concrete pc p
      | None -> get_concrete pc p )
    | If { cond; then_; else_; _ } ->
      let add_cond cond = List.map (fun (pc, v) -> (and_ pc cond, v)) in
      if pc => cond then get_object p pc then_
      else if pc => not_ cond then get_object p pc else_
      else
        let then_b, then_pc, then_pvs = get_object p (and_ pc cond) then_ in
        let else_b, else_pc, else_pvs =
          get_object p (and_ pc (not_ cond)) else_
        in

        let then_pvs, else_pvs =
          (add_cond cond then_pvs, add_cond (not_ cond) else_pvs)
        in
        let pvs' = then_pvs @ else_pvs in

        let b', s_pc' =
          match (then_b, else_b) with
          | true, true -> (true, true_)
          | true, false -> (false, and_ (not_ cond) else_pc)
          | false, true -> (false, and_ cond then_pc)
          | false, false ->
            (false, or_ (and_ cond then_pc) (and_ (not_ cond) else_pc))
        in
        (b', s_pc', pvs')
    | Empty _ -> (false, true_, [])

  and
    (* returns (b, s_pc, pvs)
       b:
        True = stop searching;
        False = continue searching
       s_pc:
        pc to consider during searching
        it returns true_ when there is nothing to consider
       pvs:
        a list of condition with the respective value
    *)
    get_object (p : value) (pc : pc_value) (o : t) :
    bool * pc_value * (pc_value * value option) list =
    List.fold_left
      (fun (r, s_pc, pvs) orec ->
        if not r then
          let r', s_pc', pvs' = get_record orec s_pc p in
          (r', and_ s_pc s_pc', pvs @ pvs')
        else (r, s_pc, pvs) )
      (false, pc, []) o

  let mk_ite_get (conds : (pc_value * value option) list) : value =
    if List.exists (fun (_, v) -> Option.is_some v) conds then
      List.fold_right
        (fun (cond, v) acc -> ite cond (Option.value v ~default:undef) acc)
        conds undef
    else undef

  let get (o : t) (field : value) (pc : pc_value) : (value * pc_value) list =
    let _, _, l = get_object field pc o in
    let ite_expr = mk_ite_get l in
    [ (ite_expr, pc) ]

  let delete (o : t) (field : value) (pc : pc_value) : (t * pc_value) list =
    let o =
      match o with
      | [ Empty _ ] -> o
      | Empty t :: tl -> create_record ~time:t () :: tl
      | _ -> o
    in
    match o with
    | Rec cur :: tl -> (
      match Expr.view field with
      | Val (Str field) ->
        Hashtbl.replace cur.concrete field None;
        [ (o, pc) ]
      | _ ->
        let new_record = Rec { cur with symbolic = Some (field, None) } in
        let empty_record = create_empty_record ~time:cur.time () in
        [ (empty_record :: new_record :: tl, pc) ] )
    | [ Empty _ ] -> [ (o, pc) ]
    | _ -> failwith "Object_wlmerge.delete: unexpected case"

  let to_list (o : t) : (value * value) list =
    (* Just calculates if there are just concrete fields *)
    match o with
    | [ Rec cur ] ->
      Hashtbl.fold
        (fun k v acc -> match v with Some v -> (str k, v) :: acc | None -> acc)
        cur.concrete []
    | [ Empty _ ] -> []
    | _ -> assert false

  let get_fields (o : t) : value list =
    (* Just calculates if there are just concrete fields *)
    match o with
    | [ Rec cur ] ->
      Hashtbl.fold
        (fun k v acc -> match v with Some _ -> str k :: acc | None -> acc)
        cur.concrete []
    | [ Empty _ ] -> []
    | _ -> assert false

  let mk_ite_has_field (conds : (pc_value * value option) list) : value =
    if List.exists (fun (_, v) -> Option.is_some v) conds then
      List.fold_right
        (fun (cond, v) acc ->
          let v' = map_default (fun _ -> true_) false_ v in
          ite cond v' acc )
        conds false_
    else false_

  let has_field (o : t) (field : value) (pc : pc_value) : value =
    let _, _, l = get_object field pc o in
    let ite_expr = mk_ite_has_field l in
    ite_expr
end
