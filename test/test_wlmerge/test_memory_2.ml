open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test case 2: Working with 5 objects *)
let () =
  let pc = value_bool true in
  let obj1 = Obj.create () in
  let obj2 = Obj.create () in
  let obj3 = Obj.create () in
  let obj4 = Obj.create () in
  let obj5 = Obj.create () in

  (* Create memory with 5 objects *)
  let m = Mem.create () in
  let loc1 = Mem.insert m obj1 in
  let loc2 = Mem.insert m obj2 in
  let loc3 = Mem.insert m obj3 in
  let loc4 = Mem.insert m obj4 in
  let loc5 = Mem.insert m obj5 in

  (* Add fields to objects *)
  let _ = Mem.set_field m loc1 ~field:(key_c "a") ~data:(value_int 100) pc in
  let _ = Mem.set_field m loc1 ~field:(key_c "b") ~data:(value_int 200) pc in
  let _ = Mem.set_field m loc2 ~field:(key_c "c") ~data:(value_int 300) pc in
  let _ = Mem.set_field m loc2 ~field:(key_c "d") ~data:(value_int 400) pc in
  let _ = Mem.set_field m loc3 ~field:(key_c "e") ~data:(value_int 500) pc in
  let _ = Mem.set_field m loc3 ~field:(key_c "f") ~data:(value_int 600) pc in
  let _ = Mem.set_field m loc4 ~field:(key_c "g") ~data:(value_int 700) pc in
  let _ = Mem.set_field m loc4 ~field:(key_c "h") ~data:(value_int 800) pc in
  let _ = Mem.set_field m loc5 ~field:(key_c "i") ~data:(value_int 900) pc in
  let _ = Mem.set_field m loc5 ~field:(key_c "j") ~data:(value_int 1000) pc in

  let obj1, pc = get_obj(Obj.set obj1 ~field:(key_c "a") ~data:(value_int 100) pc) in
  let obj1, pc = get_obj(Obj.set obj1 ~field:(key_c "b") ~data:(value_int 200) pc) in
  let obj2, pc = get_obj(Obj.set obj2 ~field:(key_c "c") ~data:(value_int 300) pc) in
  let obj2, pc = get_obj(Obj.set obj2 ~field:(key_c "d") ~data:(value_int 400) pc) in
  let obj3, pc = get_obj(Obj.set obj3 ~field:(key_c "e") ~data:(value_int 500) pc) in
  let obj3, pc = get_obj(Obj.set obj3 ~field:(key_c "f") ~data:(value_int 600) pc) in
  let obj4, pc = get_obj(Obj.set obj4 ~field:(key_c "g") ~data:(value_int 700) pc) in
  let obj4, pc = get_obj(Obj.set obj4 ~field:(key_c "h") ~data:(value_int 800) pc) in
  let obj5, pc = get_obj(Obj.set obj5 ~field:(key_c "i") ~data:(value_int 900) pc) in
  let obj5, pc = get_obj(Obj.set obj5 ~field:(key_c "j") ~data:(value_int 1000) pc) in

  assert (Mem.get m loc1 = Some obj1);
  assert (Mem.get m loc2 = Some obj2);
  assert (Mem.get m loc3 = Some obj3);
  assert (Mem.get m loc4 = Some obj4);
  assert (Mem.get m loc5 = Some obj5);

  assert (Mem.get_field m loc1 (key_c "a") pc = Obj.get obj1 (key_c "a") pc);
  assert (Mem.get_field m loc1 (key_c "b") pc = Obj.get obj1 (key_c "b") pc);
  assert (Mem.get_field m loc2 (key_c "c") pc = Obj.get obj2 (key_c "c") pc);
  assert (Mem.get_field m loc2 (key_c "d") pc = Obj.get obj2 (key_c "d") pc);
  assert (Mem.get_field m loc3 (key_c "e") pc = Obj.get obj3 (key_c "e") pc);
  assert (Mem.get_field m loc3 (key_c "f") pc = Obj.get obj3 (key_c "f") pc);
  assert (Mem.get_field m loc4 (key_c "g") pc = Obj.get obj4 (key_c "g") pc);
  assert (Mem.get_field m loc4 (key_c "h") pc = Obj.get obj4 (key_c "h") pc);
  assert (Mem.get_field m loc5 (key_c "i") pc = Obj.get obj5 (key_c "i") pc);
  assert (Mem.get_field m loc5 (key_c "j") pc = Obj.get obj5 (key_c "j") pc);

  assert (Mem.has_field m loc1 (key_c "a") pc = Obj.has_field obj1 (key_c "a") pc);
  assert (Mem.has_field m loc1 (key_c "b") pc = Obj.has_field obj1 (key_c "b") pc);
  assert (Mem.has_field m loc2 (key_c "c") pc = Obj.has_field obj2 (key_c "c") pc);
  assert (Mem.has_field m loc2 (key_c "d") pc = Obj.has_field obj2 (key_c "d") pc);
  assert (Mem.has_field m loc3 (key_c "e") pc = Obj.has_field obj3 (key_c "e") pc);
  assert (Mem.has_field m loc3 (key_c "f") pc = Obj.has_field obj3 (key_c "f") pc);
  assert (Mem.has_field m loc4 (key_c "g") pc = Obj.has_field obj4 (key_c "g") pc);
  assert (Mem.has_field m loc4 (key_c "h") pc = Obj.has_field obj4 (key_c "h") pc);
  assert (Mem.has_field m loc5 (key_c "i") pc = Obj.has_field obj5 (key_c "i") pc);
  assert (Mem.has_field m loc5 (key_c "j") pc = Obj.has_field obj5 (key_c "j") pc);

  (* has_fields on unknown fields and fields that does not exist in that object *)
  assert (Mem.has_field m loc1 (key_c "100") pc = Obj.has_field obj1 (key_c "100") pc);
  assert (Mem.has_field m loc1 (key_c "200") pc = Obj.has_field obj1 (key_c "200") pc);
  assert (Mem.has_field m loc2 (key_c "a")   pc = Obj.has_field obj2 (key_c "a")   pc);
  assert (Mem.has_field m loc2 (key_c "b")   pc = Obj.has_field obj2 (key_c "b")   pc);
  assert (Mem.has_field m loc3 (key_c "500") pc = Obj.has_field obj3 (key_c "500") pc);
  assert (Mem.has_field m loc3 (key_c "c")   pc = Obj.has_field obj3 (key_c "c")   pc);
  assert (Mem.has_field m loc4 (key_c "700") pc = Obj.has_field obj4 (key_c "700") pc);
  assert (Mem.has_field m loc4 (key_c "d")   pc = Obj.has_field obj4 (key_c "d")   pc);
  assert (Mem.has_field m loc5 (key_c "900") pc = Obj.has_field obj5 (key_c "900") pc);
  assert (
    Mem.has_field m loc5 (key_c "1000") pc = Obj.has_field obj5 (key_c "1000") pc);

  (* Delete fields in object obj1 and obj5 *)
  let obj1, pc = get_obj (Obj.delete obj1 (key_c "a") pc) in
  let obj5, pc = get_obj (Obj.delete obj5 (key_c "i") pc) in

  let _ = Mem.delete_field m loc1 (key_c "a") pc in
  let _ = Mem.delete_field m loc5 (key_c "i") pc in

  assert (Mem.get m loc1 = Some obj1);
  assert (Mem.get m loc2 = Some obj2);
  assert (Mem.get m loc3 = Some obj3);
  assert (Mem.get m loc4 = Some obj4);
  assert (Mem.get m loc5 = Some obj5);

  assert (Mem.get_field m loc1 (key_c "a") pc = Obj.get obj1 (key_c "a") pc);
  assert (Mem.get_field m loc1 (key_c "b") pc = Obj.get obj1 (key_c "b") pc);
  assert (Mem.get_field m loc2 (key_c "c") pc = Obj.get obj2 (key_c "c") pc);
  assert (Mem.get_field m loc2 (key_c "d") pc = Obj.get obj2 (key_c "d") pc);
  assert (Mem.get_field m loc3 (key_c "e") pc = Obj.get obj3 (key_c "e") pc);
  assert (Mem.get_field m loc3 (key_c "f") pc = Obj.get obj3 (key_c "f") pc);
  assert (Mem.get_field m loc4 (key_c "g") pc = Obj.get obj4 (key_c "g") pc);
  assert (Mem.get_field m loc4 (key_c "h") pc = Obj.get obj4 (key_c "h") pc);
  assert (Mem.get_field m loc5 (key_c "i") pc = Obj.get obj5 (key_c "i") pc);
  assert (Mem.get_field m loc5 (key_c "j") pc = Obj.get obj5 (key_c "j") pc);

  assert (Mem.has_field m loc1 (key_c "a") pc = Obj.has_field obj1 (key_c "a") pc);
  assert (Mem.has_field m loc1 (key_c "b") pc = Obj.has_field obj1 (key_c "b") pc);
  assert (Mem.has_field m loc2 (key_c "c") pc = Obj.has_field obj2 (key_c "c") pc);
  assert (Mem.has_field m loc2 (key_c "d") pc = Obj.has_field obj2 (key_c "d") pc);
  assert (Mem.has_field m loc3 (key_c "e") pc = Obj.has_field obj3 (key_c "e") pc);
  assert (Mem.has_field m loc3 (key_c "f") pc = Obj.has_field obj3 (key_c "f") pc);
  assert (Mem.has_field m loc4 (key_c "g") pc = Obj.has_field obj4 (key_c "g") pc);
  assert (Mem.has_field m loc4 (key_c "h") pc = Obj.has_field obj4 (key_c "h") pc);
  assert (Mem.has_field m loc5 (key_c "i") pc = Obj.has_field obj5 (key_c "i") pc);
  assert (Mem.has_field m loc5 (key_c "j") pc = Obj.has_field obj5 (key_c "j") pc);

  (* has_fields on unknown fields and fields that does not exist in that object *)
  assert (Mem.has_field m loc1 (key_c "100") pc = Obj.has_field obj1 (key_c "100") pc);
  assert (Mem.has_field m loc2 (key_c "a")   pc = Obj.has_field obj2 (key_c "a") pc);
  assert (Mem.has_field m loc3 (key_c "500") pc = Obj.has_field obj3 (key_c "500") pc);
  assert (Mem.has_field m loc4 (key_c "700") pc = Obj.has_field obj4 (key_c "700") pc);
  assert (Mem.has_field m loc5 (key_c "900") pc = Obj.has_field obj5 (key_c "900") pc);

  (* Remove all object except obj1 *)
  let _ = Mem.remove m loc2 in
  let _ = Mem.remove m loc3 in
  let _ = Mem.remove m loc4 in
  let _ = Mem.remove m loc5 in

  assert (Mem.get m loc1 = Some obj1);
  assert (Mem.get m loc2 = None);
  assert (Mem.get m loc3 = None);
  assert (Mem.get m loc4 = None);
  assert (Mem.get m loc5 = None)
