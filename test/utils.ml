open Memory_models
open Encoding
module Obj = Object_symbolic.M

let eq v1 v2 =
  (* Just accepts v1 and v2 of type int *)
  Expr.(relop Ty.Ty_int Ty.Eq v1 v2)

let ne cond = Expr.(unop Ty.Ty_bool Ty.Not cond)
let undef = Expr.(make @@ Symbol Symbol.("undefined" @: Ty.Ty_str))
let key_c str = Expr.(make @@ Val (Str str))
let key_s str = Expr.(make @@ Symbol Symbol.(str @: Ty.Ty_str))
let value_str v = Expr.(make @@ Val (Str v))
let value_int v = Expr.(make @@ Val (Int v))
let value_bool v = Expr.Bool.v v
let ite c v1 v2 = Expr.(Bool.ite c v1 v2)

let list_is_equal l1 l2 =
  let sort_l1 = List.sort compare l1 in
  let sort_l2 = List.sort compare l2 in
  sort_l1 = sort_l2

(* Create an object with n concrete fields *)
let _creat_obj_c (n : int) : Obj.t =
  let obj = Obj.create () in
  let rec set_fields obj count =
    if count <= n then
      let key = key_c (string_of_int count) in
      let data = value_int (count + 100000) in
      let obj' = Obj.set obj ~key ~data in
      set_fields obj' (count + 1)
    else obj
  in
  set_fields obj 1
