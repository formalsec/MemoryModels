open Utils
open Memory_models
module Obj = Object_symbolic.M
module Mem = Memory_symb

(* Test case 4: Working with 10 objects, with 4 parents, where each parent have 2 obj stored. *)
(* Test case 4-1: Setting fields to obj located in parent. *)
let () =
  let obj1 = Obj.create () in
  let obj2 = Obj.create () in
  let obj3 = Obj.create () in
  let obj4 = Obj.create () in
  let obj5 = Obj.create () in
  let obj6 = Obj.create () in
  let obj7 = Obj.create () in
  let obj8 = Obj.create () in
  let obj9 = Obj.create () in
  let obj10 = Obj.create () in

  (* Create memory with 2 objects in the "level 1" *)
  let m = Mem.create () in
  let loc1 = Mem.insert m obj1 in
  let loc2 = Mem.insert m obj2 in
  let _ = Mem.set_field m loc1 ~field:(key_c "a") ~data:(value_int 100) in
  let _ = Mem.set_field m loc1 ~field:(key_c "b") ~data:(value_int 200) in
  let obj1 = Obj.set obj1 ~key:(key_c "a") ~data:(value_int 100) in
  let obj1 = Obj.set obj1 ~key:(key_c "b") ~data:(value_int 200) in

  (* Create memory with 2 objects in the "level 2" *)
  let m = Mem.clone m in
  let loc3 = Mem.insert m obj3 in
  let loc4 = Mem.insert m obj4 in
  let _ = Mem.set_field m loc2 ~field:(key_c "c") ~data:(value_int 300) in
  let _ = Mem.set_field m loc2 ~field:(key_c "d") ~data:(value_int 400) in
  let _ = Mem.set_field m loc3 ~field:(key_c "e") ~data:(value_int 500) in
  let _ = Mem.set_field m loc3 ~field:(key_c "f") ~data:(value_int 600) in
  let obj2 = Obj.set obj2 ~key:(key_c "c") ~data:(value_int 300) in
  let obj2 = Obj.set obj2 ~key:(key_c "d") ~data:(value_int 400) in
  let obj3 = Obj.set obj3 ~key:(key_c "e") ~data:(value_int 500) in
  let obj3 = Obj.set obj3 ~key:(key_c "f") ~data:(value_int 600) in

  (* Create memory with 2 objects in the "level 3" *)
  let m = Mem.clone m in
  let loc5 = Mem.insert m obj5 in
  let loc6 = Mem.insert m obj6 in
  let _ = Mem.set_field m loc4 ~field:(key_c "g") ~data:(value_int 700) in
  let _ = Mem.set_field m loc4 ~field:(key_c "h") ~data:(value_int 800) in
  let _ = Mem.set_field m loc5 ~field:(key_c "i") ~data:(value_int 900) in
  let _ = Mem.set_field m loc5 ~field:(key_c "j") ~data:(value_int 1000) in
  let obj4 = Obj.set obj4 ~key:(key_c "g") ~data:(value_int 700) in
  let obj4 = Obj.set obj4 ~key:(key_c "h") ~data:(value_int 800) in
  let obj5 = Obj.set obj5 ~key:(key_c "i") ~data:(value_int 900) in
  let obj5 = Obj.set obj5 ~key:(key_c "j") ~data:(value_int 1000) in

  (* Create memory with 2 objects in the "level 4" *)
  let m = Mem.clone m in
  let loc7 = Mem.insert m obj7 in
  let loc8 = Mem.insert m obj8 in
  let _ = Mem.set_field m loc6 ~field:(key_c "k") ~data:(value_int 1100) in
  let _ = Mem.set_field m loc6 ~field:(key_c "l") ~data:(value_int 1200) in
  let obj6 = Obj.set obj6 ~key:(key_c "k") ~data:(value_int 1100) in
  let obj6 = Obj.set obj6 ~key:(key_c "l") ~data:(value_int 1200) in

  (* Create memory with 2 objects in the "level 5" *)
  let m = Mem.clone m in
  let loc9 = Mem.insert m obj9 in
  let loc10 = Mem.insert m obj10 in
  let _ = Mem.set_field m loc8 ~field:(key_c "m") ~data:(value_int 1300) in
  let _ = Mem.set_field m loc8 ~field:(key_c "n") ~data:(value_int 1400) in
  let obj8 = Obj.set obj8 ~key:(key_c "m") ~data:(value_int 1300) in
  let obj8 = Obj.set obj8 ~key:(key_c "n") ~data:(value_int 1400) in

  assert (Mem.get m loc1 = Some obj1);
  assert (Mem.get m loc2 = Some obj2);
  assert (Mem.get m loc3 = Some obj3);
  assert (Mem.get m loc4 = Some obj4);
  assert (Mem.get m loc5 = Some obj5);
  assert (Mem.get m loc6 = Some obj6);
  assert (Mem.get m loc7 = Some obj7);
  assert (Mem.get m loc8 = Some obj8);
  assert (Mem.get m loc9 = Some obj9);
  assert (Mem.get m loc10 = Some obj10);

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
  assert (Mem.get_field m loc6 (key_c "k") = Obj.get obj6 (key_c "k"));
  assert (Mem.get_field m loc6 (key_c "l") = Obj.get obj6 (key_c "l"));
  assert (Mem.get_field m loc8 (key_c "m") = Obj.get obj8 (key_c "m"));
  assert (Mem.get_field m loc8 (key_c "n") = Obj.get obj8 (key_c "n"))

(* Test case 4-1: Deleting obj located the first level.*)
let () =
  let obj1 = Obj.create () in
  let obj2 = Obj.create () in
  let obj3 = Obj.create () in
  let obj4 = Obj.create () in
  let obj5 = Obj.create () in
  let obj6 = Obj.create () in
  let obj7 = Obj.create () in
  let obj8 = Obj.create () in
  let obj9 = Obj.create () in
  let obj10 = Obj.create () in

  (* Create memory with 2 objects in the "level 1" *)
  let m = Mem.create () in
  let loc1 = Mem.insert m obj1 in
  let loc2 = Mem.insert m obj2 in
  let _ = Mem.set_field m loc1 ~field:(key_c "a") ~data:(value_int 100) in
  let _ = Mem.set_field m loc1 ~field:(key_c "b") ~data:(value_int 200) in
  let obj1 = Obj.set obj1 ~key:(key_c "a") ~data:(value_int 100) in
  let _obj1 = Obj.set obj1 ~key:(key_c "b") ~data:(value_int 200) in

  (* Create memory with 2 objects in the "level 2" *)
  let m = Mem.clone m in
  let loc3 = Mem.insert m obj3 in
  let loc4 = Mem.insert m obj4 in
  let _ = Mem.set_field m loc2 ~field:(key_c "c") ~data:(value_int 300) in
  let _ = Mem.set_field m loc2 ~field:(key_c "d") ~data:(value_int 400) in
  let _ = Mem.set_field m loc3 ~field:(key_c "e") ~data:(value_int 500) in
  let _ = Mem.set_field m loc3 ~field:(key_c "f") ~data:(value_int 600) in
  let obj2 = Obj.set obj2 ~key:(key_c "c") ~data:(value_int 300) in
  let obj2 = Obj.set obj2 ~key:(key_c "d") ~data:(value_int 400) in
  let obj3 = Obj.set obj3 ~key:(key_c "e") ~data:(value_int 500) in
  let _obj3 = Obj.set obj3 ~key:(key_c "f") ~data:(value_int 600) in

  (* Create memory with 2 objects in the "level 3" *)
  let m = Mem.clone m in
  let loc5 = Mem.insert m obj5 in
  let loc6 = Mem.insert m obj6 in
  let _ = Mem.set_field m loc4 ~field:(key_c "g") ~data:(value_int 700) in
  let _ = Mem.set_field m loc4 ~field:(key_c "h") ~data:(value_int 800) in
  let _ = Mem.set_field m loc5 ~field:(key_c "i") ~data:(value_int 900) in
  let _ = Mem.set_field m loc5 ~field:(key_c "j") ~data:(value_int 1000) in
  let obj4 = Obj.set obj4 ~key:(key_c "g") ~data:(value_int 700) in
  let obj4 = Obj.set obj4 ~key:(key_c "h") ~data:(value_int 800) in
  let obj5 = Obj.set obj5 ~key:(key_c "i") ~data:(value_int 900) in
  let _obj5 = Obj.set obj5 ~key:(key_c "j") ~data:(value_int 1000) in

  (* Create memory with 2 objects in the "level 4" *)
  let m = Mem.clone m in
  let loc7 = Mem.insert m obj7 in
  let loc8 = Mem.insert m obj8 in
  let _ = Mem.set_field m loc6 ~field:(key_c "k") ~data:(value_int 1100) in
  let _ = Mem.set_field m loc6 ~field:(key_c "l") ~data:(value_int 1200) in
  let obj6 = Obj.set obj6 ~key:(key_c "k") ~data:(value_int 1100) in
  let obj6 = Obj.set obj6 ~key:(key_c "l") ~data:(value_int 1200) in

  (* Create memory with 2 objects in the "level 5" *)
  let m = Mem.clone m in
  let loc9 = Mem.insert m obj9 in
  let loc10 = Mem.insert m obj10 in
  let _ = Mem.set_field m loc8 ~field:(key_c "m") ~data:(value_int 1300) in
  let _ = Mem.set_field m loc8 ~field:(key_c "n") ~data:(value_int 1400) in
  let obj8 = Obj.set obj8 ~key:(key_c "m") ~data:(value_int 1300) in
  let obj8 = Obj.set obj8 ~key:(key_c "n") ~data:(value_int 1400) in

  (* Delete "even" objects *)
  let _ = Mem.remove m loc1 in
  let _ = Mem.remove m loc3 in
  let _ = Mem.remove m loc5 in
  let _ = Mem.remove m loc7 in
  let _ = Mem.remove m loc9 in
  assert (Mem.get m loc1 = None);
  assert (Mem.get m loc2 = Some obj2);
  assert (Mem.get m loc3 = None);
  assert (Mem.get m loc4 = Some obj4);
  assert (Mem.get m loc5 = None);
  assert (Mem.get m loc6 = Some obj6);
  assert (Mem.get m loc7 = None);
  assert (Mem.get m loc8 = Some obj8);
  assert (Mem.get m loc9 = None);
  assert (Mem.get m loc10 = Some obj10);

  assert (Mem.get_field m loc1 (key_c "a") = []);
  assert (Mem.get_field m loc1 (key_c "b") = []);
  assert (Mem.get_field m loc2 (key_c "c") = Obj.get obj2 (key_c "c"));
  assert (Mem.get_field m loc2 (key_c "d") = Obj.get obj2 (key_c "d"));
  assert (Mem.get_field m loc3 (key_c "e") = []);
  assert (Mem.get_field m loc3 (key_c "f") = []);
  assert (Mem.get_field m loc4 (key_c "g") = Obj.get obj4 (key_c "g"));
  assert (Mem.get_field m loc4 (key_c "h") = Obj.get obj4 (key_c "h"));
  assert (Mem.get_field m loc5 (key_c "i") = []);
  assert (Mem.get_field m loc5 (key_c "j") = []);
  assert (Mem.get_field m loc6 (key_c "k") = Obj.get obj6 (key_c "k"));
  assert (Mem.get_field m loc6 (key_c "l") = Obj.get obj6 (key_c "l"));
  assert (Mem.get_field m loc8 (key_c "m") = Obj.get obj8 (key_c "m"));
  assert (Mem.get_field m loc8 (key_c "n") = Obj.get obj8 (key_c "n"))
