open Utils
open Memory_models
module Obj = Object_manuel

(* Test1: Base cases tests *)
let () =
  let x = key_s "x" in
  let _y = key_s "y" in
  let foo = key_c "foo" in
  let banana = key_c "banana" in
  let pc = value_bool true in

  (*********** Create an empty object ***********)
  Format.printf "##### Create an empty object #####\n";
  let obj = Obj.create () in
  let _ = Obj.has_field obj foo pc in

  let _ = Obj.get obj foo pc in
  let _ = Obj.get obj x pc in

  (*********** Concrete write {"foo": 100} ***********)
  Format.printf "##### Concrete write {\"foo\": 100} #####\n";
  let val_100 = value_int 100 in
  let obj, pc = get_obj (Obj.set obj ~field:foo ~data:val_100 pc) in

  let _ = Obj.get obj foo pc in
  let _ = Obj.get obj x pc in
  let _ = Obj.get obj banana pc in

  (*********** Delete field foo -> Empty object ***********)
  Format.printf "##### Delete field foo -> Empty object #####\n";
  let obj, pc = get_obj (Obj.delete obj foo pc) in

  let _ = Obj.get obj foo pc in
  let _ = Obj.get obj x pc in

  (*********** Symbolic write {x : 200} ***********)
  Format.printf "##### Symbolic write {x : 200} #####\n";
  let val_200 = value_int 200 in
  let obj, pc = get_obj (Obj.set obj ~field:x ~data:val_200 pc) in

  let _ = Obj.get obj foo pc in
  let _ = Obj.get obj x pc in

  (*********** Delete field x -> Empty object ***********)
  Format.printf "##### Delete field x -> Empty object #####\n";
  let obj, pc = get_obj (Obj.delete obj x pc) in
  let _ = Obj.get obj foo pc in
  let _ = Obj.get obj x pc in
  ()
