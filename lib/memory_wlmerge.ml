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
  let time = ref 0

  let get_new_time () =
    time := !time + 1;
    !time

  let create () : t =
    [ { map = Hashtbl.create 512; time = 0; changes = ref IntSet.empty } ]

  let pp_loc (fmt : Fmt.t) (loc : value) : unit = Expr.pp fmt loc

  let pp (fmt : Fmt.t) (h : t) =
    let open Fmt in
    let pp_set fmt set = pp_iter ~pp_sep:pp_comma IntSet.iter pp_int fmt set in
    let pp_v fmt (loc, o) = fprintf fmt "%a=>%a" pp_int loc O.pp o in
    let pp_rec fmt { map; time; changes } =
      fprintf fmt "{%a={@\n %a }; {%a}}" pp_int time
        (pp_hashtbl ~pp_sep:(fun fmt () -> fprintf fmt "@\n ") pp_v)
        map pp_set !changes
    in
    fprintf fmt "%a"
      (pp_lst ~pp_sep:(fun fmt () -> fprintf fmt "@\n->@ ") pp_rec)
      h

  let get_loc (loc : value) : int =
    match Expr.view loc with
    | Val (Int l) -> l
    | _ -> failwith "memory_wlmerge.get_loc: Not a location"

  let merge (_h1 : t) (_h2 : t) (_cond : pc_value) : t = assert false
  (* match (h1, h2) with
     | hr1 :: (h :: tl as h1'), hr2 :: h2' when h1' = h2' ->
       let changes = IntSet.union !(hr1.changes) !(hr2.changes) in
       IntSet.iter
         (fun l ->
           let o1 = Hashtbl.find hr1.map l in
           let o2 = Hashtbl.find hr2.map l in
           match (o1, o2) with
           | None, None -> failwith "TODO: merge"
           | Some _, None | None, Some _ -> failwith "TODO: merge"
           | Some o1', Some o2' ->
             let new_o = O.merge o1' o2' cond in
             Hashtbl.replace h.map l (Some new_o) )
         changes;
       { h with time = get_new_time () } :: tl
     | _ -> failwith "memory_wlmerge.merge: Unexepected cases"
  *)

  let clone (h : t) : t =
    { map = Hashtbl.create 64
    ; time = get_new_time ()
    ; changes = ref IntSet.empty
    }
    :: h

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
    let+ obj, from_parent = find_object h loc in
    match from_parent with
    | false -> obj
    | true ->
      (* TODO: *)
      let obj = O.clone obj 0 in
      set_object h loc obj;
      obj

  let has_field (h : t) (loc : value) (field : value) (pc : pc_value) : value =
    Option.fold (get_object h loc)
      ~some:(fun o -> O.has_field o field pc)
      ~none:false_

  let set (h : t) (l : value) ~(field : value) ~(data : value) (pc : pc_value) :
    unit =
    Option.iter
      (fun o ->
        match O.set o ~field ~data pc with
        | [ (o', _) ] -> set_object h l o'
        | _ ->
          failwith "memory_wlmerge.set_field: non/multiple objects returned" )
      (get_object h l)

  let get (h : t) (loc : value) (field : value) (pc : pc_value) :
    (value * pc_value) list =
    let o = get_object h loc in
    Option.fold o ~none:[] ~some:(fun o -> O.get o field pc)

  let delete (h : t) (loc : value) (f : value) (pc : pc_value) : unit =
    let obj = get_object h loc in
    Option.iter
      (fun o ->
        match O.delete o f pc with
        | [ (o', _) ] -> set_object h loc o'
        | _ ->
          failwith "memory_wlmerge.delete_field: non/multiple objects returned"
        )
      obj
end

module M :
  Memory_intf.S
    with type value = Smtml.Expr.t
     and type pc_value = Smtml.Expr.t
     and type object_ = Object_wlmerge.M.t =
  Make (Object_wlmerge.M)

include M
