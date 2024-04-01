open Utils
open Memory_models
module Obj = Object_mwl.M

(* Test1: Base cases tests *)
let () =
  let x = key_s "x" in
  let y = key_s "y" in
  let foo = key_c "foo" in
  let banana = key_c "banana" in
  let pc = value_bool true in

  (*********** Create an empty object ***********)
  let obj = Obj.create () in
  assert (Obj.get_fields obj = []);

  (*********** Concrete write {"foo": 100} ***********)
  Format.printf "\n######### Concrete Write {\"foo\" : 100}#######\n";
  let val_100 = value_int 100 in
  let obj, _ = get_obj (Obj.set obj ~field:foo ~data:val_100 pc) in

  (* print_get foo (Obj.get obj foo pc);
  print_get banana (Obj.get obj banana pc);
  print_get x (Obj.get obj x pc);
  print_get y (Obj.get obj y pc); *)

  (*********** Symbolic write {x: 200} ***********)
  Format.printf "\n######### Symbolic Write {x : 200}#######\n";
  let val_200 = value_int 200 in
  let obj, _ = get_obj (Obj.set obj ~field:x ~data:val_200 pc) in

  (* TODO: If x = foo 200 else 100 *)
  print_get foo (Obj.get obj foo pc);
  print_get banana (Obj.get obj banana pc);
  print_get x (Obj.get obj x pc);
  print_get y (Obj.get obj y pc);

  Format.printf "\n######### Symbolic Write {x : 300}#######\n";
  let val_300 = value_int 300 in
  let obj, _ = get_obj (Obj.set obj ~field:x ~data:val_300 pc) in

  (* TODO: If x = foo 200 else 100 *)
  print_get foo (Obj.get obj foo pc);
  print_get banana (Obj.get obj banana pc);
  print_get x (Obj.get obj x pc);
  print_get y (Obj.get obj y pc)