open Utils.Encoding
open Encoding

module Make (O : Object_intf.S with type value = Encoding.Expr.t and type pc_value = Encoding.Expr.t) = struct
  module IntSet = Set.Make(Int)
  type value = Encoding.Expr.t
  type pc_value = Encoding.Expr.t
  type object_ = O.t

  type heap_rec = { map: (object_ option, CCVector.rw) CCVector.t; time: int; changes: IntSet.t }
  type t = heap_rec list

  let loc = ref 0

  let create () : t = [{map = CCVector.create (); time = 0; changes = IntSet.empty}]

  let pp_loc (fmt : Fmt.t) (loc : value) : unit = Expr.pp fmt loc

  let pp (fmt : Fmt.t) (h : t) =
    let open Fmt in
    let pp_v fmt data = fprintf fmt "%a" (pp_opt O.pp) data in
    let pp_rec fmt { map; time; _ } =
      fprintf fmt "%a{ %a }" pp_int time (pp_vector ~pp_sep:pp_comma pp_v) map
    in
    fprintf fmt "%a" (pp_lst ~pp_sep:(fun fmt () -> fprintf fmt "->@") pp_rec) h

  let get_loc (loc : value) : int =
    match Expr.view loc with
    | Val (Int l) -> l
    | _ -> failwith "memory_wlmerge.get_loc: Not a location"

  let clone (h : t) : t =
    let top_h = List.hd h in
    {map = CCVector.create (); time = top_h.time; changes = IntSet.empty} :: h

  let insert (h : t) (o : object_) : value =
    let h = List.hd h in
    let next = !loc in
    (* FIXME: this insert is dangerous because it may change indexes *)
    CCVector.insert h.map next (Some o);
    loc := !loc + 1;
    Expr.(make @@ Val (Int next))

  let remove (h : t) (l : value) : unit =
    let h = List.hd h in
    let loc = get_loc l in
    CCVector.set h.map loc None

  let set ( h : t) (l : value) (o : object_) : unit =
    let h = List.hd h in
    let loc = get_loc l in
    CCVector.set h.map loc (Some o)

  let find (h : t) (l : value) : (object_ * bool) option =
    let loc = get_loc l in
    let rec aux h loc from_parent = 
      match h with
      | [] -> None
      | { map; _ } :: tl ->
        try
        match CCVector.get map loc with
        | Some o -> Some (o, from_parent)
        | None -> aux tl loc true
        with Invalid_argument _ -> aux tl loc true
      in 
    aux h loc false

  let get (h : t) (loc : value) : object_ option =
    let open Utils.Option in
    let+ obj, from_parent = find h loc in
    match from_parent with
    | false -> obj
    | true ->
      let obj = O.clone obj in
      set h loc obj;
      obj

  let has_field (h : t) (loc : value) (field : value) (pc : pc_value) : value =
    Option.fold (get h loc)
      ~some:(fun o -> O.has_field o field pc)
      ~none:(boolean false)

  let set_field (h : t) (loc : value) ~(field : value) ~(data : value)
    (pc : pc_value) : unit =
    Option.iter
      (fun o ->
        match O.set o ~field ~data pc with
        | [ (o', _)] ->  set h loc o'
        | _ -> failwith "memory_wlmerge.set_field: non/multiple objects returned")
      (get h loc)

  let get_field (h : t) (loc : value) (field : value) (pc : pc_value) :
    (value * pc_value) list =
    let o = get h loc in
    Option.fold o ~none:[] ~some:(fun o -> O.get o field pc)

  let delete_field (h : t) (loc : value) (f : value) (pc : pc_value) : unit =
    let obj = get h loc in
    Option.iter
      (fun o ->
        match O.delete o f pc with
        | [ (o', _)] -> set h loc o'
        | _ -> failwith "memory_wlmerge.delete_field: non/multiple objects returned")
      obj
end

module M :
  Memory_intf.S
    with type value = Encoding.Expr.t
    and type pc_value = Encoding.Expr.t
     and type object_ = Object_wlmerge.M.t =
  Make (Object_wlmerge.M)

include M
