open Utils.Encoding
open Utils.Option
open Smtml

module Make
    (O : Object_intf.S
           with type value = Smtml.Expr.t
            and type pc_value = Smtml.Expr.t) =
struct
  module IntSet = Set.Make (Int)

  type value = Smtml.Expr.t
  type pc_value = Smtml.Expr.t
  type object_ = O.t

  type heap_rec = (int, object_) Hashtbl.t

  type t = heap_rec list

  let loc = ref 0
  let create () : t =
    [ Hashtbl.create 512 ]

  let pp_loc (fmt : Fmt.t) (loc : int) : unit = Fmt.pp_int fmt loc

  let pp (fmt : Fmt.t) (h : t) =
    let open Fmt in
    let pp_v fmt (loc, o) = fprintf fmt "%a=>%a" pp_int loc O.pp o in
    let pp_rec fmt map =
      fprintf fmt "{@\n %a }"
        (pp_hashtbl ~pp_sep:(fun fmt () -> fprintf fmt "@\n ") pp_v)
        map
    in
    fprintf fmt "%a"
      (pp_lst ~pp_sep:(fun fmt () -> fprintf fmt "@\n->@ ") pp_rec)
      h

  let get_loc (loc : value) : int =
    match Expr.view loc with
    | Val (Int l) -> l
    | _ -> failwith "memory_mwl.get_loc: Not a location"

  let single_merge (_ : t) (_ : int) (_ : pc_value) : t =
    failwith "memory_mwl.single_merge: Not implemented"
  
  let merge (_ : t) (_ : t) (_ : int) (_ : pc_value) : t =
    failwith "memory_mwl.merge: Not implemented"

  let clone (h : t) _: t =
    Hashtbl.create 64 :: h

  let alloc (h : t) : value =
    let map = List.hd h in
    let next = !loc in
    let o = O.create () in
    Hashtbl.replace map next o;
    loc := !loc + 1;
    Expr.(make @@ Val (Int next))

  let set_object (h : t) (l : value) (o : object_) : unit =
    let h = List.hd h in
    let loc = get_loc l in
    Hashtbl.replace h loc o

  let find_object (h : t) (l : value) : (object_ * bool) option =
    let loc = get_loc l in
    let rec aux h loc from_parent =
      match h with
      | [] -> None
      | map :: tl -> 
          match Hashtbl.find_opt map loc with
          | Some o -> Some (o, from_parent)
          | None -> aux tl loc true
    in
    aux h loc false

  let get_object (h : t) (loc : value) : object_ option =
    let+ obj, from_parent = find_object h loc in
    match from_parent with
    | false -> obj
    | true ->
      let obj = O.clone obj in
      set_object h loc obj;
      obj

  let has_field (h : t) (loc : value) (field : value) (pc : pc_value) : value =
    Option.fold (get_object h loc) ~some:(fun o -> O.has_field o field pc) ~none:false_

  let set (h : t) (l : value) ~(field : value) ~(data : value)
    (pc : pc_value) : unit =
    Option.iter
      (fun o ->
        match O.set o ~field ~data pc with
        | [ (o', _) ] -> set_object h l o'
        | _ ->
          failwith "memory_mwl.set_field: non/multiple objects returned" )
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
          failwith "memory_mwl.delete_field: non/multiple objects returned"
        )
      obj

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
     and type object_ = Object_mwl.M.t =
  Make (Object_mwl.M)

include M