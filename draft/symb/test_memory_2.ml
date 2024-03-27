open Utils
open Memory_models
module Obj = Object_symbolic.M
module Mem = Memory_symb

(* Test case 2: Working with 5 objects *)
let () =
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
  let _ = Mem.set_field m loc1 ~field:(key_c "a") ~data:(value_int 100) in
  let _ = Mem.set_field m loc1 ~field:(key_c "b") ~data:(value_int 200) in
  let _ = Mem.set_field m loc2 ~field:(key_c "c") ~data:(value_int 300) in
  let _ = Mem.set_field m loc2 ~field:(key_c "d") ~data:(value_int 400) in
  let _ = Mem.set_field m loc3 ~field:(key_c "e") ~data:(value_int 500) in
  let _ = Mem.set_field m loc3 ~field:(key_c "f") ~data:(value_int 600) in
  let _ = Mem.set_field m loc4 ~field:(key_c "g") ~data:(value_int 700) in
  let _ = Mem.set_field m loc4 ~field:(key_c "h") ~data:(value_int 800) in
  let _ = Mem.set_field m loc5 ~field:(key_c "i") ~data:(value_int 900) in
  let _ = Mem.set_field m loc5 ~field:(key_c "j") ~data:(value_int 1000) in

  let obj1 = Obj.set obj1 ~key:(key_c "a") ~data:(value_int 100) in
  let obj1 = Obj.set obj1 ~key:(key_c "b") ~data:(value_int 200) in
  let obj2 = Obj.set obj2 ~key:(key_c "c") ~data:(value_int 300) in
  let obj2 = Obj.set obj2 ~key:(key_c "d") ~data:(value_int 400) in
  let obj3 = Obj.set obj3 ~key:(key_c "e") ~data:(value_int 500) in
  let obj3 = Obj.set obj3 ~key:(key_c "f") ~data:(value_int 600) in
  let obj4 = Obj.set obj4 ~key:(key_c "g") ~data:(value_int 700) in
  let obj4 = Obj.set obj4 ~key:(key_c "h") ~data:(value_int 800) in
  let obj5 = Obj.set obj5 ~key:(key_c "i") ~data:(value_int 900) in
  let obj5 = Obj.set obj5 ~key:(key_c "j") ~data:(value_int 1000) in

  assert (Mem.get m loc1 = Some obj1);
  assert (Mem.get m loc2 = Some obj2);
  assert (Mem.get m loc3 = Some obj3);
  assert (Mem.get m loc4 = Some obj4);
  assert (Mem.get m loc5 = Some obj5);

  assert (Mem.get_field m loc1 (key_c "a") = Obj.get obj1 (key_c "a"));
  assert (Mem.get_field m loc1 (key_c "b") = Obj.get obj1 (key_c "b"));
  assert (Mem.get_field m loc2 (key_c "c") = Obj.get obj2 (key_c "c"));
  assert (Mem.get_field m loc2 (key_c "d") = Obj.get obj2 (key_c "d"));
  assert (Mem.get_field m loc3 (key_c "e") = Obj.get obj3 (key_c "e"));
  assert (Mem.get_field m loc3 (key_c "f") = Obj.get obj3 (key_c "f"));
  assert (Mem.get_field m loc4 (key_c "g") = Obj.get obj4 (key_c "g"));
  assert (Mem.get_field m loc4 (key_c "h") = Obj.get obj4 (key_c "h"));
  assert (Mem.get_field m loc5 (key_c "i") = Obj.get obj5 (key_c "i"));
  assert (Mem.get_field m loc5 (key_c "j") = Obj.get obj5 (key_c "j"));

  assert (Mem.has_field m loc1 (key_c "a") = Obj.has_field obj1 (key_c "a"));
  assert (Mem.has_field m loc1 (key_c "b") = Obj.has_field obj1 (key_c "b"));
  assert (Mem.has_field m loc2 (key_c "c") = Obj.has_field obj2 (key_c "c"));
  assert (Mem.has_field m loc2 (key_c "d") = Obj.has_field obj2 (key_c "d"));
  assert (Mem.has_field m loc3 (key_c "e") = Obj.has_field obj3 (key_c "e"));
  assert (Mem.has_field m loc3 (key_c "f") = Obj.has_field obj3 (key_c "f"));
  assert (Mem.has_field m loc4 (key_c "g") = Obj.has_field obj4 (key_c "g"));
  assert (Mem.has_field m loc4 (key_c "h") = Obj.has_field obj4 (key_c "h"));
  assert (Mem.has_field m loc5 (key_c "i") = Obj.has_field obj5 (key_c "i"));
  assert (Mem.has_field m loc5 (key_c "j") = Obj.has_field obj5 (key_c "j"));

  (* has_fields on unknown fields and fields that does not exist in that object *)
  assert (Mem.has_field m loc1 (key_c "100") = Obj.has_field obj1 (key_c "100"));
  assert (Mem.has_field m loc1 (key_c "200") = Obj.has_field obj1 (key_c "200"));
  assert (Mem.has_field m loc2 (key_c "a") = Obj.has_field obj2 (key_c "a"));
  assert (Mem.has_field m loc2 (key_c "b") = Obj.has_field obj2 (key_c "b"));
  assert (Mem.has_field m loc3 (key_c "500") = Obj.has_field obj3 (key_c "500"));
  assert (Mem.has_field m loc3 (key_c "c") = Obj.has_field obj3 (key_c "c"));
  assert (Mem.has_field m loc4 (key_c "700") = Obj.has_field obj4 (key_c "700"));
  assert (Mem.has_field m loc4 (key_c "d") = Obj.has_field obj4 (key_c "d"));
  assert (Mem.has_field m loc5 (key_c "900") = Obj.has_field obj5 (key_c "900"));
  assert (
    Mem.has_field m loc5 (key_c "1000") = Obj.has_field obj5 (key_c "1000") );

  (* Delete fields in object obj1 and obj5 *)
  let obj1 = Obj.delete obj1 (key_c "a") in
  let obj5 = Obj.delete obj5 (key_c "i") in

  let _ = Mem.delete_field m loc1 (key_c "a") in
  let _ = Mem.delete_field m loc5 (key_c "i") in

  assert (Mem.get m loc1 = Some obj1);
  assert (Mem.get m loc2 = Some obj2);
  assert (Mem.get m loc3 = Some obj3);
  assert (Mem.get m loc4 = Some obj4);
  assert (Mem.get m loc5 = Some obj5);

  assert (Mem.get_field m loc1 (key_c "a") = Obj.get obj1 (key_c "a"));
  assert (Mem.get_field m loc1 (key_c "b") = Obj.get obj1 (key_c "b"));
  assert (Mem.get_field m loc2 (key_c "c") = Obj.get obj2 (key_c "c"));
  assert (Mem.get_field m loc2 (key_c "d") = Obj.get obj2 (key_c "d"));
  assert (Mem.get_field m loc3 (key_c "e") = Obj.get obj3 (key_c "e"));
  assert (Mem.get_field m loc3 (key_c "f") = Obj.get obj3 (key_c "f"));
  assert (Mem.get_field m loc4 (key_c "g") = Obj.get obj4 (key_c "g"));
  assert (Mem.get_field m loc4 (key_c "h") = Obj.get obj4 (key_c "h"));
  assert (Mem.get_field m loc5 (key_c "i") = Obj.get obj5 (key_c "i"));
  assert (Mem.get_field m loc5 (key_c "j") = Obj.get obj5 (key_c "j"));

  assert (Mem.has_field m loc1 (key_c "a") = Obj.has_field obj1 (key_c "a"));
  assert (Mem.has_field m loc1 (key_c "b") = Obj.has_field obj1 (key_c "b"));
  assert (Mem.has_field m loc2 (key_c "c") = Obj.has_field obj2 (key_c "c"));
  assert (Mem.has_field m loc2 (key_c "d") = Obj.has_field obj2 (key_c "d"));
  assert (Mem.has_field m loc3 (key_c "e") = Obj.has_field obj3 (key_c "e"));
  assert (Mem.has_field m loc3 (key_c "f") = Obj.has_field obj3 (key_c "f"));
  assert (Mem.has_field m loc4 (key_c "g") = Obj.has_field obj4 (key_c "g"));
  assert (Mem.has_field m loc4 (key_c "h") = Obj.has_field obj4 (key_c "h"));
  assert (Mem.has_field m loc5 (key_c "i") = Obj.has_field obj5 (key_c "i"));
  assert (Mem.has_field m loc5 (key_c "j") = Obj.has_field obj5 (key_c "j"));

  (* has_fields on unknown fields and fields that does not exist in that object *)
  assert (Mem.has_field m loc1 (key_c "100") = Obj.has_field obj1 (key_c "100"));
  assert (Mem.has_field m loc2 (key_c "a") = Obj.has_field obj2 (key_c "a"));
  assert (Mem.has_field m loc3 (key_c "500") = Obj.has_field obj3 (key_c "500"));
  assert (Mem.has_field m loc4 (key_c "700") = Obj.has_field obj4 (key_c "700"));
  assert (Mem.has_field m loc5 (key_c "900") = Obj.has_field obj5 (key_c "900"));

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
