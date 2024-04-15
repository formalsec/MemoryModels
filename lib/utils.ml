module Option = struct
  let ( let* ) v f = Option.bind v f
  let ( let+ ) v f = Option.map f v
  let map_default f d = function None -> d | Some v -> f v
end

module List = struct
  let map_default f d = function [] -> d | l -> f l

  let split_while xs ~f =
    let rec loop acc = function
      | hd :: tl when f hd -> loop (hd :: acc) tl
      | t -> (List.rev acc, t)
    in
    loop [] xs
end

module Encoding = struct
  open Encoding

  type t = Expr.t

  let solver = Solver.Z3_batch.create ()
  let undef = Expr.(make @@ Symbol Symbol.("undefined" @: Ty.Ty_str))
  let str s = Expr.(make @@ Val (Str s))
  let boolean v = Expr.(Bool.v v)
  let ite c v1 v2 = Expr.(Bool.ite c v1 v2)
  let not_ = Expr.Bool.not
  let eq v1 v2 = Expr.(relop Ty.Ty_bool Ty.Eq v1 v2)
  let ne v1 v2 = Expr.(unop Ty.Ty_bool Ty.Not (eq v1 v2))
  let and_ v1 v2 = Expr.Bool.and_ v1 v2
  let or_ v1 v2 = Expr.Bool.or_ v1 v2
  let is_val v = match Expr.view v with Val _ -> true | _ -> false
  let is_sat (exprs : t list) : bool = Solver.Z3_batch.check solver exprs
  let ( => ) (e1 : t) (e2 : t) : bool = not (is_sat [ and_ e1 (not_ e2) ])
end
