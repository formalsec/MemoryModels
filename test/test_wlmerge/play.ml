open Test.Utils
open Memory_models.Utils.Encoding
open Memory_models
open Encoding

type pc_value = Encoding.Expr.t
type value = Encoding.Expr.t
type symb_slot = (value * value option) option
type concrete_table = (string, value option) Hashtbl.t

type record =
  | Rec of { concrete : concrete_table; symbolic : symb_slot; id : int }
  | If of { cond : pc_value; then_ : t; else_ : t }

and t = record list

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
let _to_json = to_string
let id = ref 0

let get_new_id () =
  id := !id + 1;
  !id

let create_record ?(id = 0) () : record =
  Rec { concrete = Hashtbl.create 16; symbolic = None; id }

let _create_if_record (cond : pc_value) (then_ : t) (else_ : t) : record =
  If { cond; then_; else_ }

let create () : t = [ create_record () ]

let _clone (o : t) : t =
  let new_rec = create_record ~id:(get_new_id ()) () in
  new_rec :: o

let _merge (_o1 : t) (_o2 : t) (_pc : pc_value) : t =
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
    let keys = Memory_models.Hashtbl.keys concrete in
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
  let open Memory_models.Utils.List in
  let r = Option.get r in
  let add_cond lst cond = List.map (fun (pc, v) -> (and_ pc cond, v)) lst in
  Format.printf "get_aux: %a@." pp o;

  match o with
  | [] -> (None, pvs)
  | (Rec _ as cur) :: parent -> (
    let r', pvs' = get_record cur r p in
    let pvs'' = pvs @ pvs' in
    match r' with
    | None -> (None, pvs'')
    | Some b ->
      map_default (get_aux p pc (Some (and_ r b), pvs'')) (None, pvs'') parent )
  | If { cond; then_; else_ } :: parent -> (
    Format.printf "then: %a@." pp then_;
    let then_r, then_pvs = get_aux p (and_ pc cond) (Some r, []) then_ in
    let else_r, else_pvs = get_aux p (and_ pc (not_ cond)) (Some r, []) else_ in
    Format.printf "then %a: %a@."
      (Fmt.pp_opt (fun fmt _ -> Fmt.fprintf fmt "Some"))
      then_r
      (Fmt.pp_lst ~pp_sep:Fmt.pp_semicolon (fun fmt (pc, v) ->
           Fmt.fprintf fmt "(%a, %a)" Expr.pp pc Expr.pp
             (Option.value v ~default:undef) ) )
      then_pvs;
    Format.printf "else %a: %a@."
      (Fmt.pp_opt (fun fmt _ -> Fmt.fprintf fmt "Some"))
      else_r
      (Fmt.pp_lst ~pp_sep:Fmt.pp_semicolon (fun fmt (pc, v) ->
           Fmt.fprintf fmt "(%a, %a)" Expr.pp pc Expr.pp
             (Option.value v ~default:undef) ) )
      else_pvs;
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
  Format.printf "\n############\n get: %a\n#############@."
    (Fmt.pp_lst ~pp_sep:Fmt.pp_semicolon (fun fmt (pc, v) ->
         Fmt.fprintf fmt "(%a, %a)" Expr.pp pc Expr.pp
           (Option.value v ~default:undef) ) )
    l;
  let ite_expr = mk_ite_get l in
  [ (ite_expr, pc) ]

let _delete (o : t) (field : value) (pc : pc_value) : (t * pc_value) list =
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

let _to_list (o : t) : (value * value) list =
  (* Just calculates if there are just concrete fields *)
  match o with
  | [ Rec cur ] ->
    Hashtbl.fold
      (fun k v acc -> match v with Some v -> (str k, v) :: acc | None -> acc)
      cur.concrete []
  | _ -> assert false

let _get_fields (o : t) : value list =
  (* Just calculates if there are just concrete fields *)
  match o with
  | [ Rec cur ] ->
    Hashtbl.fold
      (fun k v acc -> match v with Some _ -> str k :: acc | None -> acc)
      cur.concrete []
  | _ -> assert false

let mk_ite_has_field (conds : (pc_value * value option) list) : value =
  let open Memory_models.Utils.Option in
  if List.exists (fun (_, v) -> Option.is_some v) conds then
    List.fold_right
      (fun (cond, v) acc ->
        let v' = map_default (fun _ -> boolean true) (boolean false) v in
        ite cond v' acc )
      conds (boolean false)
  else boolean false

let _has_field (o : t) (field : value) (pc : pc_value) : value =
  let _, l = get_aux field pc (Some pc, []) o in
  let ite_expr = mk_ite_has_field l in
  ite_expr

let () =
  let x = key_s "x" in
  let y = key_s_int "y" in
  let z = key_s "z" in
  let a = key_c "a" in
  let b = key_c "b" in
  let c = key_c "c" in
  let d = key_c "d" in
  let val_3 = value_int 3 in
  let val_4 = value_int 4 in
  let val_5 = value_int 5 in
  let val_6 = value_int 6 in
  let pc = value_bool true in
  let cond = gt y (value_int 3) in

  (*
     o = {}
     o.a = 3;
     o[#x] = 4
     ------------obj1---------> R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
     if (#y > 3) {
       o.b = 4;
       o[#z] = 3;
     ------------obj2---------> R4{{};_;1};R3{{b: 4}; #z: 3; 1}; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
     } else {
       o.c = 5;
     ------------obj3---------> R5{{c:5};_;2}; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
     }
     o.d = 6
     ------------obj4---------> R6{{d:6};_} -> [ (R4, #y > 3); (R5, #y <= 3) ]

     R6{{d:6};_}; [ (#y > 3) then R4{{};_;1};R3{{b: 4}; #z: 3; 1} else R5{{c:5};_;2} ]; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}

     o.a --> [#y > 3 && #z = a, 3]; [#x = a, 4]; [True, 3]  -> ITE(#y > 3 && #z = a, 3, ITE(#x = a, 4, 3))
  *)
  let obj1 = create () in
  let obj1, pc = get_obj (set obj1 ~field:a ~data:val_3 pc) in
  let obj1, pc = get_obj (set obj1 ~field:x ~data:val_4 pc) in

  let obj2 = create () in
  let obj2, pc = get_obj (set obj2 ~field:b ~data:val_4 pc) in
  let obj2, pc = get_obj (set obj2 ~field:z ~data:val_3 pc) in

  let obj3 = create () in
  let obj3, pc = get_obj (set obj3 ~field:c ~data:val_5 pc) in

  let obj4 = create () in
  let obj4, pc = get_obj (set obj4 ~field:d ~data:val_6 pc) in

  let obj = obj4 @ [ If { cond; then_ = obj2; else_ = obj3 } ] @ obj1 in
  print_get a (get obj a pc)
