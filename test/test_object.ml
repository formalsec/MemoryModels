open Memory_models
open Encoding
module Obj = Object_symbolic.M

let key_c str = Expr.(make @@ Val (Str str))
let key_s str = Expr.(make @@ Symbol Symbol.(str @: Ty.Ty_str))
let value_int v = Expr.(make @@ Val (Int v))
let value_str v = Expr.(make @@ Val (Str v))
let _value_real v = Expr.(make @@ Val (Real v))
let value_bool v = Expr.Bool.v v

(* Base cases tests *)
let () =
  (* Create an empty object *)
  let obj = Obj.create () in
  assert (Obj.is_empty obj);
  assert (Obj.get_fields obj = []);

  (* Concrete write {"foo": 100} *)
  let foo = key_c "foo" in
  let val_100 = value_int 100 in
  let obj = Obj.set obj ~key:foo ~data:val_100 in
  assert (not (Obj.is_empty obj));
  assert (Obj.has_field obj foo = value_bool true);
  assert (Obj.get_fields obj = [ foo ]);
  assert (Obj.get obj foo = [ (val_100, []) ]);

  (* Delete field foo *)
  let obj = Obj.delete obj foo in
  assert (Obj.is_empty obj);
  assert (Obj.has_field obj foo = value_bool false);
  assert (Obj.get_fields obj = []);
  assert (Obj.get obj foo = []);

  (* Symbolic write {x : "bar"} *)
  let x = key_s "x" in
  let val_bar = value_str "bar" in
  let obj = Obj.set obj ~key:x ~data:val_bar in
  assert (not (Obj.is_empty obj));
  assert (Obj.get_fields obj = [ x ]);
  assert (Obj.get obj x = [ (val_bar, []) ])
