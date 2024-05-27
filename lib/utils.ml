module Option = struct
  let ( let* ) v f = Option.bind v f
  let ( let+ ) v f = Option.map f v
  let map_default f d = function None -> d | Some v -> f v
end

module Encoding = struct
  open Smtml

  type t = Expr.t

  let solver = Solver.Z3_batch.create ()
  let undef = Expr.(make @@ Val (App (`Op "symbol", [ Str "undefined" ])))
  let str s = Expr.(make @@ Val (Str s))
  let true_ = Expr.(Bool.v true)
  let false_ = Expr.(Bool.v false)
  let ite c v1 v2 = Expr.(Bool.ite c v1 v2)
  let not_ = Expr.Bool.not
  let eq v1 v2 = Expr.(relop Ty.Ty_bool Ty.Eq v1 v2)
  let ne v1 v2 = Expr.(unop Ty.Ty_bool Ty.Not (eq v1 v2))
  let and_ v1 v2 = Expr.Bool.and_ v1 v2
  let or_ v1 v2 = Expr.Bool.or_ v1 v2
  let is_val v = match Expr.view v with Val _ -> true | _ -> false

  let is_sat (exprs : t list) : bool =
    match Solver.Z3_batch.check solver exprs with
    | `Sat -> true
    | `Unsat -> false
    | `Unknown ->
      Format.eprintf "Unknown exprs: %a@." Smtml.Expr.pp_list exprs;
      assert false

  let ( => ) (e1 : t) (e2 : t) : bool = not (is_sat [ and_ e1 (not_ e2) ])
end
