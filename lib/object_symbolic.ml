open Encoding

let eq v1 v2 =
  (* Just accepts v1 and v2 of type int *) Expr.(relop Ty.Ty_int Ty.Eq v1 v2)

let ne v1 v2 = Expr.(unop Ty.Ty_bool Ty.Not (eq v1 v2))
let ite c v1 v2 = Expr.(Bool.ite c v1 v2)
let undef = Expr.(make @@ Symbol Symbol.("undefined" @: Ty.Ty_str))
let is_val v = match Expr.view v with Val _ -> true | _ -> false
let boolean v = Expr.(Bool.v v)

module Value_key = struct
  type t = Encoding.Expr.t

  let hash (e : t) = Hashtbl.hash e
  let t_of_sexp _ = assert false
  let sexp_of_t _ = assert false
  let compare (e1 : t) (e2 : t) = compare (Hashtbl.hash e1) (Hashtbl.hash e2)
end

module M : Object_intf.S with type value = Encoding.Expr.t = struct
  module VMap = Map.Make (Value_key)

  type value = Encoding.Expr.t
  type t = { fields : value VMap.t; symbols : value VMap.t }

  let create () = { fields = VMap.empty; symbols = VMap.empty }

  let clone o =
    (* Immutable structures don't need to be copied *)
    o

  let is_empty o = VMap.(is_empty o.fields && is_empty o.symbols)
  let to_list o = VMap.bindings o.symbols @ VMap.bindings o.fields

  let get_fields o =
    let symbols = VMap.fold (fun key _ acc -> key :: acc) o.symbols [] in
    VMap.fold (fun key _ acc -> key :: acc) o.fields symbols

  let has_field o k =
    if VMap.is_empty o.fields && VMap.is_empty o.symbols then boolean false
    else
      match Expr.view k with
      | Val _ -> boolean (VMap.mem k o.fields)
      | _ ->
          let r0 =
            VMap.fold
              (fun key _ acc -> ite (eq k key) (boolean true) acc)
              o.symbols (boolean false)
          in
          VMap.fold
            (fun key _ acc -> ite (eq k key) (boolean true) acc)
            o.fields r0

  let map_ite (m : value VMap.t) ~(key : value) ~(data : value) =
    VMap.mapi
      (fun key0 data0 ->
        if Expr.(equal key key0) then data0 else ite (eq key key0) data data0)
      m

  let set o ~key ~data =
    match Expr.view key with
    | Val _ -> { o with fields = VMap.add key data o.fields }
    | _ ->
        {
          fields = map_ite o.fields ~key ~data;
          symbols = map_ite o.symbols ~key ~data |> VMap.add key data;
        }

  let fold_eq (m : value VMap.t) (key0 : value) : (value * value) list =
    VMap.fold
      (fun key data acc ->
        if Expr.(equal key0 key) then acc else (data, eq key0 key) :: acc)
      m []

  (* FIXME: @174 *)
  let get { fields; symbols } key =
    match Expr.view key with
    | Val _ -> (
        match VMap.find_opt key fields with
        | Some v -> [ (v, []) ]
        | None -> (
            match fold_eq symbols key with
            | [] -> []
            | [ (v, cond) ] ->
                [ (v, [ cond ]); (undef, [ Expr.Bool.not cond ]) ]
            | (v0, cond0) :: tl ->
                let v, neg_conds =
                  List.fold_left
                    (fun (acc, neg_conds) (v1, cond1) ->
                      (ite cond1 v1 acc, Expr.Bool.not cond1 :: neg_conds))
                    (v0, [ Expr.Bool.not cond0 ])
                    tl
                in
                [ (v, []); (undef, neg_conds) ]))
    | _ -> (
        match VMap.find_opt key symbols with
        | Some v -> [ (v, []) ]
        | None -> (
            match fold_eq fields key with
            | [] -> []
            | [ (v, cond) ] ->
                [ (v, [ cond ]); (undef, [ Expr.Bool.not cond ]) ]
            | (v0, cond0) :: tl ->
                let v, neg_conds =
                  List.fold_left
                    (fun (acc, neg_conds) (v1, cond1) ->
                      (ite cond1 v1 acc, Expr.Bool.not cond1 :: neg_conds))
                    (v0, [ Expr.Bool.not cond0 ])
                    tl
                in
                [ (v, []); (undef, neg_conds) ]))

  let delete o key =
    match Expr.view key with
    | Val _ -> { o with fields = VMap.remove key o.fields }
    | _ -> assert false

  let pp_map fmt v =
    let open Fmt in
    let map_iter f m = VMap.iter (fun k d -> f (k, d)) m in
    pp_iter ~pp_sep:pp_comma map_iter
      (fun fmt (key, data) -> fprintf fmt {|"%a": %a|} Expr.pp key Expr.pp data)
      fmt v

  let pp fmt { fields; symbols } =
    Fmt.fprintf fmt "{ %a, %a }" pp_map fields pp_map symbols

  let to_string o = Fmt.asprintf "%a" pp o
  let to_json = to_string
end
