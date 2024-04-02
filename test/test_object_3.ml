open Utils
open Memory_models
module Obj = Object_mwl.M

(* Test case 3: Object with 1 symbolic field followed by 1 concrete field *)
let () =
  let x = key_s "x" in
  let y = key_s "y" in
  let foo = key_c "foo" in
  let banana = key_c "banana" in
  let val_bar = value_str "bar" in
  let val_one = value_str "one" in
  let val_two = value_str "two" in
  let pc = value_bool true in

  (*********** Create object {x: "one"} ***********)
  let obj = Obj.create () in
  let obj, pc = get_obj (Obj.set obj ~field:x ~data:val_one pc) in

  (*********** Concrete write {foo : "bar"} -> {"foo": "bar"} ; {x : "one"} ***********)
  let obj, pc = get_obj (Obj.set obj ~field:foo ~data:val_bar pc) in

  assert (Obj.has_field obj foo pc = value_bool true);
  assert (
    Obj.has_field obj x pc = ite (eq x foo) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj banana pc
    = ite (eq banana x) (value_bool true) (value_bool false) );
  assert (
    Obj.has_field obj y pc
    = ite (eq y foo) (value_bool true)
        (ite (eq y x) (value_bool true) (value_bool false)) );

  assert (Obj.get obj foo pc = [ (val_bar, pc) ]);
  assert (Obj.get obj x pc = [ (ite (eq x foo) val_bar val_one, pc) ]);
  assert (Obj.get obj banana pc = [ (ite (eq banana x) val_one undef, pc) ]);
  assert (
    Obj.get obj y pc
    = [ (ite (eq y foo) val_bar (ite (eq y x) val_one undef), pc) ] );

  (*********** Symbolic write {y : "two"} ***********)
  (* {} ;
     {"foo": "bar"; y : "two"};
     {x : "one"} *)
  let obj, pc = get_obj (Obj.set obj ~field:y ~data:val_two pc) in

  assert (
    Obj.has_field obj foo pc
    = ite (eq foo y) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj x pc
    = ite (eq x y) (value_bool true)
        (ite (eq x foo) (value_bool true) (value_bool true)) );
  assert (
    Obj.has_field obj banana pc
    = ite (eq banana y) (value_bool true)
        (ite (eq banana x) (value_bool true) (value_bool false)) );
  assert (Obj.has_field obj y pc = value_bool true);

  assert (Obj.get obj foo pc = [ (ite (eq foo y) val_two val_bar, pc) ]);
  assert (
    Obj.get obj x pc
    = [ (ite (eq x y) val_two (ite (eq x foo) val_bar val_one), pc) ] );
  assert (
    Obj.get obj banana pc
    = [ (ite (eq banana y) val_two (ite (eq banana x) val_one undef), pc) ] );
  assert (Obj.get obj y pc = [ (val_two, pc) ])
