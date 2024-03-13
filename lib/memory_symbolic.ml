module Make (O : Object_intf.S with type value = V.value) = struct
  type loc  = int
  type object_ = O.t

  type t =
    { parent : t option
    ; data : (Loc.t, object_) Hashtbl.t
    ; mutable next : loc
    }

  type value = V.value

  let create () : t = { parent = None; data = Hashtbl.create 512 }
  let clone (m : t) : t = { parent = Some m; data = Hashtbl.create 16 }

  let insert ({ data = memory; _ } : t) (o : object_) : value =
    let loc = Loc.create () in
    Hashtbl.replace memory loc o;
    V.Val (Val.Loc loc)

  let remove (m : t) (l : Loc.t) : unit = Hashtbl.remove m.data l

  let set ({ data = memory; _ } : t) (key : Loc.t) (data : object_) : unit =
    Hashtbl.replace memory key data

  let find memory l =
    let open Syntax.Option in
    let rec aux { parent; data } l from_parent =
      match Hashtbl.find_opt data l with
      | Some o -> Some (o, from_parent)
      | None ->
        let* parent in
        aux parent l true
    in
    aux memory l false

  let get memory l =
    let open Syntax.Option in
    let+ (obj, from_parent) = find memory l in
    match from_parent with
    | false -> obj
    | true ->
      let obj = O.clone obj in
      set memory l obj;
      obj

  let has_field (h : t) (loc : Loc.t) (field : value) : value =
    Option.fold (get h loc)
      ~some:(fun o -> O.has_field o field)
      ~none:(V.Bool.const false)

  let set_field (h : t) (loc : Loc.t) ~(field : value) ~(data : value) : unit =
    Option.iter
      (fun o ->
        let o' = O.set o ~key:field ~data in
        set h loc o' )
      (get h loc)

  let get_field (h : t) (loc : Loc.t) (field : value) :
    (value * value list) list =
    let o = get h loc in
    Option.fold o ~none:[] ~some:(fun o -> O.get o field)

  let delete_field (h : t) (loc : Loc.t) (f : value) =
    let obj = get h loc in
    Option.iter
      (fun o ->
        let o' = O.delete o f in
        set h loc o' )
      obj

  let rec pp fmt ({ data; parent } : t) =
    let open Fmt in
    let pp_v fmt (key, data) = fprintf fmt "%a: %a" Loc.pp key O.pp data in
    let pp_parent fmt v =
      pp_opt (fun fmt h -> fprintf fmt "%a@ <-@ " pp h) fmt v
    in
    fprintf fmt "%a{ %a }" pp_parent parent (pp_hashtbl ", " pp_v) data

  let rec unfold_ite ~(accum : value) (e : value) : (value option * string) list
      =
    let open V in
    let open Operator in
    match e with
    | Val (Val.Loc x) | Val (Val.Symbol x) -> [ (Some accum, x) ]
    | TriOpt (ITE, c, Val (Val.Loc l), e) ->
      let accum' = BinOpt (LogicalAnd, accum, UnOpt (LogicalNot, c)) in
      let tl = unfold_ite ~accum:accum' e in
      (Some (BinOpt (LogicalAnd, accum, c)), l) :: tl
    | _ -> assert false

  let loc (e : value) : ((value option * string) list, string) Result.t =
    let open V in
    match e with
    | Val (Val.Loc l) -> Ok [ (None, l) ]
    | TriOpt (Operator.ITE, c, Val (Val.Loc l), v) ->
      Ok ((Some c, l) :: unfold_ite ~accum:(UnOpt (Operator.LogicalNot, c)) v)
    | _ -> Error (Fmt.asprintf "Value '%a' is not a loc expression" V.pp e)

  let pp_val (h : t) (e : value) : string =
    match e with
    | V.Val (Val.Loc l) -> (
      match get h l with
      | None -> l
      | Some o -> Fmt.asprintf "%s -> %a" l O.pp o )
    | _ -> Fmt.asprintf "%a" V.pp e
end

module M :
  Memory_intf.S with type value = V.value and type object_ = Symbolic_object.M.t =
  Make (Symbolic_object.M)

include M