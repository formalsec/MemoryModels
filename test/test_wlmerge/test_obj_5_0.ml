open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M

(* Test case 5-0: pc = (x = "foo" or x = "bar")
   (       , {"foo": None})
   (y : 500, {"bar": 400 })
   (x : 300, {"foo": 100; "bar": 200; "age": 10}) *)

let () =
  let x = key_s "x" in
  let y = key_s "y" in
  let z = key_s "z" in
  let foo = key_c "foo" in
  let banana = key_c "banana" in
  let age = key_c "age" in
  let bar = key_c "bar" in
  let val_10 = value_int 10 in
  let val_100 = value_int 100 in
  let val_200 = value_int 200 in
  let val_300 = value_int 300 in
  let val_400 = value_int 400 in
  let val_500 = value_int 500 in
  let pc = or_ (eq x foo) (eq x bar) in

  let obj = Obj.create () in
  let obj, pc = get_obj (Obj.set obj ~field:foo ~data:val_100 pc) in
  let obj, pc = get_obj (Obj.set obj ~field:bar ~data:val_200 pc) in
  let obj, pc = get_obj (Obj.set obj ~field:age ~data:val_10 pc) in
  let obj, pc = get_obj (Obj.set obj ~field:x ~data:val_300 pc) in

  let obj, pc = get_obj (Obj.set obj ~field:bar ~data:val_400 pc) in
  let obj, pc = get_obj (Obj.set obj ~field:y ~data:val_500 pc) in

  let obj, pc = get_obj (Obj.delete obj foo pc) in

  (* test has_field *)
  assert (Obj.has_field obj foo pc = value_bool false);
  assert (
    Obj.has_field obj bar pc
    = ite (eq bar y) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj age pc
    = ite (eq age y) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj x pc
    = ite (eq x foo) (value_bool false)
        (ite (eq x y) (value_bool true) (value_bool true)) );
  assert (
    Obj.has_field obj y pc = ite (eq y foo) (value_bool false) (value_bool true) );
  assert (
    Obj.has_field obj banana pc
    = ite (eq banana y) (value_bool true) (value_bool false) );
  assert (
    Obj.has_field obj z pc
    = ite (eq z foo) (value_bool false)
        (ite (eq z y) (value_bool true)
           (ite (eq z bar) (value_bool true)
              (ite (eq z age) (value_bool true) (value_bool false)) ) ) );

  (* test get *)
  assert (Obj.get obj foo pc = [ (undef, pc) ]);
  assert (Obj.get obj bar pc = [ (ite (eq bar y) val_500 val_400, pc) ]);
  assert (Obj.get obj age pc = [ (ite (eq age y) val_500 val_10, pc) ]);
  assert (
    Obj.get obj x pc
    = [ (ite (eq x foo) undef (ite (eq x y) val_500 val_400), pc) ] );
  assert (Obj.get obj y pc = [ (ite (eq y foo) undef val_500, pc) ]);
  assert (Obj.get obj banana pc = [ (ite (eq banana y) val_500 undef, pc) ]);
  assert (
    Obj.get obj z pc
    = [ ( ite (eq z foo) undef
            (ite (eq z y) val_500
               (ite (eq z bar) val_400 (ite (eq z age) val_10 undef)) )
        , pc )
      ] )
