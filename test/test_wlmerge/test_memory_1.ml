open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test case 1: Base cases tests using just one object *)
let () =
  let pc = value_bool true in

  (*********** Create an empty memory and insert an empty object ***********)
  let m = Mem.create () in
  let obj = Obj.create () in
  let loc = Mem.alloc m obj in

  (*********** Concrete write {"foo" : 100} ***********)
  let foo = key_c "foo" in
  let val_100 = value_int 100 in
  let obj, pc = get_obj (Obj.set obj ~field:foo ~data:val_100 pc) in
  let _ = Mem.set m loc ~field:foo ~data:val_100 pc in
  assert (Mem.has_field m loc foo pc = Obj.has_field obj foo pc);
  assert (Mem.get m loc foo pc = Obj.get obj foo pc);

  (*********** Delete field foo ***********)
  Mem.delete m loc foo pc;
  let obj, pc = get_obj (Obj.delete obj foo pc) in
  assert (Mem.has_field m loc foo pc = Obj.has_field obj foo pc);
  assert (Mem.get m loc foo pc = Obj.get obj foo pc);

  (*********** Symbolic write {x : 200} ***********)
  let x = key_s "x" in
  let val_200 = value_int 200 in
  let _ = Mem.set m loc ~field:x ~data:val_200 pc in
  let obj, pc = get_obj (Obj.set obj ~field:x ~data:val_200 pc) in
  assert (Mem.has_field m loc x pc = Obj.has_field obj x pc);
  assert (Mem.get m loc x pc = Obj.get obj x pc)
