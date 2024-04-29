open Smtml
open Memory_models

let eq v1 v2 = Expr.(relop Ty.Ty_bool Ty.Eq v1 v2)
let ne v1 v2 = Expr.(unop Ty.Ty_bool Ty.Not (eq v1 v2))
let undef = Expr.(make @@ Symbol Symbol.("undefined" @: Ty.Ty_str))
let key_c str = Expr.(make @@ Val (Str str))
let key_s str = Expr.(make @@ Symbol Symbol.(str @: Ty.Ty_str))
let key_s_int n = Expr.(make @@ Symbol Symbol.(n @: Ty.Ty_int))
let value_str v = Expr.(make @@ Val (Str v))
let value_int v = Expr.(make @@ Val (Int v))
let value_bool v = Expr.Bool.v v
let ite c v1 v2 = Expr.(Bool.ite c v1 v2)
let or_ = Expr.Bool.or_
let and_ = Expr.Bool.and_
let not_ = Expr.Bool.not
let lt v1 v2 = Expr.(relop Ty.Ty_int Ty.Lt v1 v2)
let gt v1 v2 = Expr.(relop Ty.Ty_int Ty.Gt v1 v2)
let plus v1 v2 = Expr.(binop Ty.Ty_int Ty.Add v1 v2)

let print_get (field : Smtml.Expr.t) l (* (expr : Smtml.Expr.t) *) =
  (* Format.printf "---- get %a : %a ----\n" Smtml.Expr.pp field
     Smtml.Expr.pp expr *)
  Format.printf "---- get %a : %a ----\n" Smtml.Expr.pp field
    (Fmt.pp_lst ~pp_sep:Fmt.pp_semicolon (fun fmt (k, v) ->
         Format.fprintf fmt "(%a, %a)" Smtml.Expr.pp k Smtml.Expr.pp v ) )
    l

let print_obj pp_obj obj =
  Format.printf "\n--------\n    obj \n--------\n\n%a \n" pp_obj obj

let get_obj l =
  match l with
  | [] -> failwith "empty list"
  | [ x ] -> x
  | _ :: _ -> failwith "more than one element"

let list_is_equal l1 l2 =
  let sort_l1 = List.sort compare l1 in
  let sort_l2 = List.sort compare l2 in
  sort_l1 = sort_l2
