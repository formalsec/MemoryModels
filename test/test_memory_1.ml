open Utils
open Memory_models
module Obj = Object_symbolic.M
module Mem = Memory_symbolic

(* Test case 1: Base cases tests using just one object *)
let () =
  (*********** Create an empty memory ***********)
  let m = Mem.create () in
  let obj = Obj.create () in
  let loc = Mem.insert m obj in
  assert (Mem.get m loc = Some obj);

  (*********** Concrete write {"foo" : 100} ***********)
  let foo = key_c "foo" in
  let val_100 = value_int 100 in
  let _ = Mem.set_field m loc ~field:foo ~data:val_100 in
  assert (Mem.has_field m loc foo = value_bool true);
  assert (Mem.get_field m loc foo = [ (val_100, []) ]);
  assert (Mem.get m loc = Some (Obj.set obj ~key:foo ~data:val_100));

  (*********** Delete field foo ***********)
  Mem.delete_field m loc foo;
  let obj = Obj.delete obj foo in
  assert (Mem.has_field m loc foo = Obj.has_field obj foo);
  assert (Mem.get_field m loc foo = Obj.get obj foo);
  assert (Mem.get m loc = Some obj);

  (*********** Symbolic write {x : 200} ***********)
  let x = key_s "x" in
  let val_200 = value_int 200 in
  let _ = Mem.set_field m loc ~field:x ~data:val_200 in
  let obj = Obj.set obj ~key:x ~data:val_200 in
  assert (Mem.has_field m loc x = Obj.has_field obj x);
  assert (Mem.get_field m loc x = Obj.get obj x);
  assert (Mem.get m loc = Some obj);

  (*********** Remove object ***********)
  Mem.remove m loc;
  assert (Mem.get m loc = None);
  assert (Mem.get_field m loc foo = []);

  (*********** Set object ***********)
  let obj2 = Obj.create () in
  let _ = Mem.set m loc obj2 in
  assert (Mem.get m loc = Some obj2);
  assert (Mem.get_field m loc foo = [])
