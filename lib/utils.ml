module Option = struct
  let ( let* ) v f = Option.bind v f
  let ( let+ ) v f = Option.map f v
  let map_default f d = function None -> d | Some v -> f v
end

module Encoding = struct
  open Encoding

  type t = Expr.t

  let solver = Solver.Z3_batch.create ()
  let str s = Expr.(make @@ Val (Str s))
  let boolean v = Expr.(Bool.v v)
  let undef = Expr.(make @@ Symbol Symbol.("undefined" @: Ty.Ty_str))
  let ite c v1 v2 = Expr.(Bool.ite c v1 v2)
  let neg cond = Expr.(unop Ty.Ty_bool Ty.Not cond)

  let eq v1 v2 =
    (* Just accepts v1 and v2 of type int *)
    Expr.(relop Ty.Ty_int Ty.Eq v1 v2)

  let ne v1 v2 = Expr.(unop Ty.Ty_bool Ty.Not (eq v1 v2))

  let and_ v1 v2 =
    (* Expr.(binop Ty.Ty_bool Ty.And v1 v2) *) Expr.Bool.and_ v1 v2

  let or_ v1 v2 = Expr.(binop Ty.Ty_bool Ty.Or v1 v2)
  let is_val v = match Expr.view v with Val _ -> true | _ -> false
  let is_sat (exprs : t list) : bool = Solver.Z3_batch.check solver exprs

  let ( => ) (e1 : t) (e2 : t) : bool =
    (* Format.printf "%%%%%% %a implies %a%%%%%%\n" Expr.pp e1 Expr.pp e2; *)
    is_sat [ Expr.Bool.or_ (neg e1) e2 ]
end
