module Option = struct
  let ( let* ) v f = Option.bind v f
  let ( let+ ) v f = Option.map f v
  let map_default f d = function None -> d | Some v -> f v
end

module Encoding = struct
  open Encoding

  type t = Expr.t

  let solver = Solver.Z3_batch.create ()
  let is_sat (exprs : t list) : bool = Solver.Z3_batch.check solver exprs
  let neg cond = Expr.(unop Ty.Ty_bool Ty.Not cond)
  let ( => ) (e1 : t) (e2 : t) : bool = is_sat [ Expr.Bool.or_ (neg e1) e2 ]
  let str s = Expr.(make @@ Val (Str s))

  let eq v1 v2 =
    (* Just accepts v1 and v2 of type int *)
    Expr.(relop Ty.Ty_int Ty.Eq v1 v2)

  let ne v1 v2 = Expr.(unop Ty.Ty_bool Ty.Not (eq v1 v2))
  let ite c v1 v2 = Expr.(Bool.ite c v1 v2)
  let undef = Expr.(make @@ Symbol Symbol.("undefined" @: Ty.Ty_str))
  let is_val v = match Expr.view v with Val _ -> true | _ -> false
  let boolean v = Expr.(Bool.v v)
end
