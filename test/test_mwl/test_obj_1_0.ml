open Test.Utils
open Memory_models
module Obj = Object_mwl.M

(* Test 1: pc = true
    Base cases tests *)
let () =
  let x = key_s "x" in
  let y = key_s "y" in
  let foo = key_c "foo" in
  let banana = key_c "banana" in
  let pc = value_bool true in

  (*********** Create an empty object ***********)
  let obj = Obj.create () in
  assert (Obj.to_list obj = []);
  assert (Obj.get_fields obj = []);
  assert (Obj.has_field obj foo pc = value_bool false);
  assert (Obj.has_field obj x pc = value_bool false);
  assert (Obj.get obj foo pc = [ (undef, pc) ]);
  assert (Obj.get obj x pc = [ (undef, pc) ]);
  assert (get_obj (Obj.delete obj foo pc) = (obj, pc));

  (*********** Concrete write {"foo": 100} ***********)
  let val_100 = value_int 100 in
  let obj, pc = get_obj (Obj.set obj ~field:foo ~data:val_100 pc) in

  assert (Obj.to_list obj = [ (foo, val_100) ]);
  assert (Obj.get_fields obj = [ foo ]);
  assert (Obj.has_field obj foo pc = value_bool true);
  assert (Obj.has_field obj banana pc = value_bool false);
  assert (
    Obj.has_field obj x pc = ite (eq x foo) (value_bool true) (value_bool false) );

  assert (Obj.get obj foo pc = [ (val_100, pc) ]);
  assert (Obj.get obj banana pc = [ (undef, pc) ]);
  assert (Obj.get obj x pc = [ (ite (eq x foo) val_100 undef, pc) ]);

  (*********** Delete field foo -> {"foo": None} -> Empty object ***********)
  let obj, pc = get_obj (Obj.delete obj foo pc) in

  assert (Obj.to_list obj = []);
  assert (Obj.get_fields obj = []);
  assert (Obj.has_field obj foo pc = value_bool false);
  assert (Obj.has_field obj x pc = value_bool false);

  assert (Obj.get obj foo pc = [ (undef, pc) ]);
  assert (Obj.get obj x pc = [ (undef, pc) ]);

  (*********** Symbolic write {x : 200} -> {}; {"foo": None; x : 200} ***********)
  let val_200 = value_int 200 in
  let obj, pc = get_obj (Obj.set obj ~field:x ~data:val_200 pc) in

  assert (
    Obj.has_field obj foo pc
    = ite (eq foo x) (value_bool true) (value_bool false) );
  assert (Obj.has_field obj x pc = value_bool true);
  assert (
    Obj.has_field obj y pc
    = ite (eq y x) (value_bool true)
        (ite (eq y foo) (value_bool false) (value_bool false)) );

  assert (Obj.get obj x pc = [ (val_200, pc) ]);
  assert (Obj.get obj foo pc = [ (ite (eq foo x) val_200 undef, pc) ]);

  (*********** Delete field x -> {x : None}; {"foo": None; x : 200} -> Empty object ***********)
  let obj, pc = get_obj (Obj.delete obj x pc) in

  assert (Obj.has_field obj foo pc = value_bool false);
  assert (Obj.has_field obj x pc = value_bool false);

  assert (Obj.get obj foo pc = [ (undef, pc) ]);
  assert (Obj.get obj x pc = [ (undef, pc) ])
