open Encoding

module Make (O : Object_intf.S with type value = Encoding.Expr.t) = struct
  type value = Encoding.Expr.t
  type loc = int
  type object_ = O.t

  type t =
    { parent : t option
    ; map : (loc, object_) Hashtbl.t
    ; mutable next : loc
    }

  let create () : t = { parent = None; map = Hashtbl.create 512; next = 0 }
  let clone (h : t) : t = { parent = Some h; map = Hashtbl.create 16; next = 0 }

  let insert (h : t) (o : object_) : value =
    Hashtbl.replace h.map h.next o;
    h.next <- h.next + 1;
    Expr.(make @@ Val (Int h.next))

  let remove (h : t) (loc : loc) : unit = Hashtbl.remove h.map loc

  let set ({ map = h; _ } : t) (loc : loc) (data : object_) : unit =
    Hashtbl.replace h loc data

  let find (h : t) (loc : loc) : (object_ * bool) option =
    let open Utils.Option in
    let rec aux { parent; map; _ } loc from_parent =
      match Hashtbl.find_opt map loc with
      | Some o -> Some (o, from_parent)
      | None ->
        let* parent in
        aux parent loc true
    in
    aux h loc false

  let get (h : t) (loc : loc) : object_ option =
    let open Utils.Option in
    let+ obj, from_parent = find h loc in
    match from_parent with
    | false -> obj
    | true ->
      let obj = O.clone obj in
      set h loc obj;
      obj

  let has_field (h : t) (loc : loc) (field : value) : value =
    Option.fold (get h loc)
      ~some:(fun o -> O.has_field o field)
      ~none:Expr.(Bool.v false)

  let set_field (h : t) (loc : loc) ~(field : value) ~(data : value) : unit =
    Option.iter
      (fun o ->
        let o' = O.set o ~key:field ~data in
        set h loc o' )
      (get h loc)

  let get_field (h : t) (loc : loc) (field : value) : (value * value list) list
      =
    let o = get h loc in
    Option.fold o ~none:[] ~some:(fun o -> O.get o field)

  let delete_field (h : t) (loc : loc) (f : value) =
    let obj = get h loc in
    Option.iter
      (fun o ->
        let o' = O.delete o f in
        set h loc o' )
      obj

  let rec unfold_ite ~(accum : value) (e : value) : (value option * loc) list =
    match Expr.view e with
    | Val (Int x) (* | Val (Val.Symbol x)  *) -> [ (Some accum, x) ]
    | Triop (_, Ty.Ite, c, a, e) -> (
      match Expr.view a with
      | Val (Int l) ->
        let accum' =
          Expr.(binop Ty.Ty_bool Ty.And accum (unop Ty.Ty_bool Ty.Not c))
        in
        let tl = unfold_ite ~accum:accum' e in
        (Some Expr.(binop Ty.Ty_bool Ty.And accum c), l) :: tl
      | _ -> assert false )
    | _ -> assert false

  let loc (e : value) : ((value option * loc) list, string) Result.t =
    match Expr.view e with
    | Val (Int l) -> Ok [ (None, l) ]
    | Triop (_, Ty.Ite, c, a, v) -> (
      match Expr.view a with
      | Val (Int l) ->
        Ok ((Some c, l) :: unfold_ite ~accum:Expr.(unop Ty.Ty_bool Ty.Not c) v)
      | _ -> Error (Fmt.asprintf "Value '%a' is not a loc expression" Expr.pp e)
      )
    | _ -> Error (Fmt.asprintf "Value '%a' is not a loc expression" Expr.pp e)

  let pp_loc (fmt : Fmt.t) (loc : loc) : unit = Fmt.pp_int fmt loc

  let rec pp (fmt : Fmt.t) ({ map; parent; _ } : t) =
    let open Fmt in
    let pp_v fmt (key, data) = fprintf fmt "%a: %a" pp_loc key O.pp data in
    let pp_parent fmt v =
      pp_opt (fun fmt h -> fprintf fmt "%a@ <-@ " pp h) fmt v
    in
    fprintf fmt "%a{ %a }" pp_parent parent
      (pp_hashtbl ~pp_sep:pp_comma pp_v)
      map

  let pp_val (fmt : Fmt.t) (h : t) (e : value) : unit =
    let open Fmt in
    match Expr.view e with
    | Val (Int l) -> (
      match get h l with
      | None -> fprintf fmt "%a" pp_loc l
      | Some o -> fprintf fmt "%a -> %a" pp_loc l O.pp o )
    | _ -> fprintf fmt "%a" Expr.pp e
end

module M :
  Memory_intf.S
    with type value = Encoding.Expr.t
     and type object_ = Object_symbolic.M.t =
  Make (Object_symbolic.M)

include M
