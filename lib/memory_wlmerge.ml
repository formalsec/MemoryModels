open Utils.Encoding
open Utils.Option
open Smtml

module Make
    (O : Object_intf2.S
           with type value = Smtml.Expr.t
            and type pc_value = Smtml.Expr.t) =
struct
  module IntSet = Set.Make (Int)

  type value = Smtml.Expr.t
  type pc_value = Smtml.Expr.t
  type object_ = O.t

  type heap_rec =
    { map : (int, object_) Hashtbl.t; time : int; changes : IntSet.t ref }

  type t = heap_rec list

  let loc = ref 0

  let create () : t =
    [ { map = Hashtbl.create 512; time = 0; changes = ref IntSet.empty } ]

  let pp_loc (fmt : Fmt.t) (loc : int) : unit = Fmt.pp_int fmt loc

  let pp (fmt : Fmt.t) (h : t) =
    let open Fmt in
    let pp_set fmt set = pp_iter ~pp_sep:pp_comma IntSet.iter pp_int fmt set in
    let pp_v fmt (loc, o) = fprintf fmt "%a -> %a" pp_int loc O.pp o in
    let pp_rec fmt { map; time; changes } =
      fprintf fmt "{%a=(%a){@\n %a }}" pp_int time pp_set !changes
        (pp_hashtbl ~pp_sep:(fun fmt () -> fprintf fmt "@\n ") pp_v)
        map
    in
    fprintf fmt "%a"
      (pp_lst ~pp_sep:(fun fmt () -> fprintf fmt "@\n->@ ") pp_rec)
      h

  let rec get_locs_aux (pc : pc_value) (s_pc : pc_value) (v : value) :
    (value * pc_value) list =
    let ( &&& ) e1 e2 = Expr.Bool.and_ e1 e2 in
    let not e1 = Expr.Bool.not e1 in

    match Expr.view v with
    | Val (Int _) -> [ (v, s_pc) ]
    | Triop (_, Ty.Ite, c, e1, e2) ->
      if pc => c then get_locs_aux pc s_pc e1
      else if pc => not c then get_locs_aux pc s_pc e2
      else
        let rets1 = get_locs_aux pc (s_pc &&& c) e1 in
        let rets2 = get_locs_aux pc (s_pc &&& not c) e2 in
        rets1 @ rets2
    | _ -> failwith "Error: Invalid Location"

  let get_loc (loc : value) : int =
    match Expr.view loc with
    | Val (Int l) -> l
    | _ -> failwith "memory_wlmerge.get_loc: Not a location"

  let merge (h1 : t) (h2 : t) (time : int) (cond : pc_value) : t =
    let single_merge h h' cond_ =
      IntSet.iter (fun l ->
          let o' = Hashtbl.find h'.map l in
          let new_o = O.single_merge o' time cond_ in
          Hashtbl.replace h.map l new_o )
    in

    match (h1, h2) with
    | hr1 :: (h :: _ as h1'), hr2 :: h2' when h1' = h2' ->
      (* merge *)
      let changes = IntSet.inter !(hr1.changes) !(hr2.changes) in
      IntSet.iter
        (fun l ->
          let o1 = Hashtbl.find hr1.map l in
          let o2 = Hashtbl.find hr2.map l in
          let new_o = O.merge o1 o2 time cond in
          Hashtbl.replace h.map l new_o )
        changes;

      (* single merge then *)
      let changes = IntSet.diff !(hr1.changes) !(hr2.changes) in
      single_merge h hr1 cond changes;

      (* single merge else *)
      let changes = IntSet.diff !(hr2.changes) !(hr1.changes) in
      single_merge h hr2 (not_ cond) changes;

      let new_changes = IntSet.union !(hr1.changes) !(hr2.changes) in
      h.changes := IntSet.union new_changes !(h.changes);
      h1'
    | _ -> failwith "memory_wlmerge.merge: Unexepected cases"

  let clone (h : t) (time : int) : t =
    { map = Hashtbl.create 64; time; changes = ref IntSet.empty } :: h

  let alloc (h : t) : value =
    let h = List.hd h in
    let next = !loc in
    let o = O.create () in
    (* FIXME: Se houver a função delete object entao tem de ser o object_ option  *)
    Hashtbl.replace h.map next o;
    loc := !loc + 1;
    h.changes := IntSet.add next !(h.changes);
    Expr.(make @@ Val (Int next))

  let set_object (h : t) (l : value) (o : object_) : unit =
    let h = List.hd h in
    let loc = get_loc l in
    Hashtbl.replace h.map loc o;
    h.changes := IntSet.add loc !(h.changes)

  let find_object (h : t) (l : value) : (object_ * bool) option =
    let loc = get_loc l in
    let rec aux h loc from_parent =
      match h with
      | [] -> None
      | { map; _ } :: tl -> (
        match Hashtbl.find_opt map loc with
        | Some o -> Some (o, from_parent)
        | None -> aux tl loc true )
    in
    aux h loc false

  let get_object (h : t) (loc : value) : object_ option =
    let time = match h with [] -> 0 | { time; _ } :: _ -> time in
    let+ obj, from_parent = find_object h loc in
    match from_parent with
    | false -> obj
    | true ->
      let obj = O.clone obj time in
      set_object h loc obj;
      obj

  let has_field (h : t) (loc : value) (field : value) (pc : pc_value) : value =
    Option.fold (get_object h loc)
      ~some:(fun o -> O.has_field o field pc)
      ~none:false_

  let set_aux (h : t) (l : value) ~(field : value) ~(data : value)
    ?(cond : pc_value option = None) (pc : pc_value) : unit =
    Option.iter
      (fun o ->
        let set_list =
          match cond with
          | None -> O.set o ~field ~data pc
          | Some cond -> O.set_conditional o ~field ~data pc cond
        in
        match set_list with
        | [ (o', _) ] -> set_object h l o'
        | _ ->
          failwith "memory_wlmerge.set_field: non/multiple objects returned" )
      (get_object h l)

  let set (h : t) (l : value) ~(field : value) ~(data : value) (pc : pc_value) :
    unit =
    let locs = get_locs_aux pc true_ l in
    match locs with
    | [ (loc, _) ] -> set_aux h loc ~field ~data pc
    | _ ->
      List.iter
        (fun (loc, cond) ->
          set_aux h loc ~field ~data ~cond:(Some cond) (and_ pc cond) )
        locs

  let mk_ite_get (conds : (value * pc_value) list) : value =
    List.fold_right (fun (v, cond) acc -> ite cond v acc) conds undef

  let get_aux (h : t) (loc : value) (field : value) (pc : pc_value) :
    (value * pc_value) list =
    let o = get_object h loc in
    Option.fold o ~none:[] ~some:(fun o -> O.get o field pc)

  let get (h : t) (loc : value) (field : value) (pc : pc_value) :
    (value * pc_value) list =
    let locs = get_locs_aux pc true_ loc in
    (* lista de (locs, pc), sem ter a pc geral em conta *)
    let values =
      List.concat_map
        (fun (loc, cond) ->
          let rets = get_aux h loc field (and_ pc cond) in
          List.map (fun (v, _) -> (v, cond)) rets )
        locs
    in
    [ (mk_ite_get values, pc) ]

  (* ite(y>10, loc(1), loc(2)); pc = true
     get_aux loc(1) field (pc=true and y>10) -> [(value1, pc)]
     get_aux loc(2) field (pc=y<=10) -> [(value2, pc)]

     [(value1, y>10), (value2, y<=10)]
     ite(y>10, value1, ite(y<=10, value2, undef))
  *)

  let delete_aux (h : t) (loc : value) (f : value)
    ?(cond : pc_value option = None) (pc : pc_value) : unit =
    let obj = get_object h loc in
    Option.iter
      (fun o ->
        let delete_list =
          match cond with
          | None -> O.delete o f pc
          | Some cond -> O.delete_conditional o f pc cond
        in
        match delete_list with
        | [ (o', _) ] -> set_object h loc o'
        | _ ->
          failwith "memory_wlmerge.delete_field: non/multiple objects returned"
        )
      obj

  let delete (h : t) (loc : value) (f : value) (pc : pc_value) : unit =
    let locs = get_locs_aux pc true_ loc in
    match locs with
    | [ (loc, _) ] -> delete_aux h loc f pc
    | _ ->
      List.iter
        (fun (loc, cond) -> delete_aux h loc f ~cond:(Some cond) (and_ pc cond))
        locs

  let get_fields (h : t) (loc : value) : value list =
    let obj = get_object h loc in
    Option.fold obj ~none:[] ~some:O.get_fields

  let to_list (h : t) (loc : value) : (value * value) list =
    let obj = get_object h loc in
    Option.fold obj ~none:[] ~some:O.to_list

  let pp_val (h : t) (fmt : Fmt.t) (loc : value) : unit =
    let open Fmt in
    match Expr.view loc with
    | Val (Int l) -> (
      match get_object h loc with
      | None -> fprintf fmt "%a" pp_loc l
      | Some o -> Fmt.fprintf fmt "%a -> %a" pp_loc l O.pp o )
    | _ -> Fmt.fprintf fmt "%a" Expr.pp loc
end

module M :
  Memory_intf.S
    with type value = Smtml.Expr.t
     and type pc_value = Smtml.Expr.t
     and type object_ = Object_wlmerge.M.t =
  Make (Object_wlmerge.M)

include M
