open Memory_models
open Encoding
module Obj = Object_symbolic.M
module Mem = Memory_symbolic

let key_c str = Expr.(make @@ Val (Str str))
let _key_s str = Expr.(make @@ Symbol Symbol.(str @: Ty.Ty_str))
let value_int v = Expr.(make @@ Val (Int v))
let _value_str v = Expr.(make @@ Val (Str v))
let _value_real v = Expr.(make @@ Val (Real v))
let value_bool v = Expr.Bool.v v

(* Base cases tests *)
let () =
  (* Create an empty memory *)
  let m = Mem.create () in
  let obj = Obj.create () in
  let loc = Mem.insert m obj in
  assert (Mem.get m loc = Some obj);

  (* Concrete write {"foo":100} *)
  let foo = key_c "foo" in
  let val_100 = value_int 100 in
  let _ = Mem.set_field m loc ~field:foo ~data:val_100 in
  assert (Mem.has_field m loc foo = value_bool true);
  assert (Mem.get_field m loc foo = [ (val_100, []) ]);
  assert (Mem.get m loc = Some (Obj.set obj ~key:foo ~data:val_100));

  (* Delete field foo *)
  Mem.delete_field m loc foo;
  assert (Mem.has_field m loc foo = value_bool false);
  assert (Mem.get_field m loc foo = []);
  assert (Mem.get m loc = Some (Obj.delete obj foo))
