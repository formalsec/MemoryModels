open Utils
open Memory_models
module Obj = Object_mwl.M

(* Test case 2: Object with 1 concrete field followed by 1 symbolic field *)
let () =
  let x = key_s "x" in
  let y = key_s "y" in
  let foo = key_c "foo" in
  let banana = key_c "banana" in
  let val_100 = value_int 100 in
  let val_200 = value_int 200 in
  let val_300 = value_int 300 in
  let val_400 = value_int 400 in
  let pc = value_bool true in

  (*********** Create object {"foo": 100} ***********)
  let obj = Obj.create () in
  let obj, pc = get_obj (Obj.set obj ~field:foo ~data:val_100 pc) in
  assert (list_is_equal (Obj.to_list obj) [ (foo, val_100) ]);
  assert (list_is_equal (Obj.get_fields obj) [ foo ]);

  (*********** Symbolic write {x : 200} -> {} ; {"foo": 100; x : 200}  ***********)
  let obj, pc = get_obj (Obj.set obj ~field:x ~data:val_200 pc) in
  assert (list_is_equal (Obj.get_fields obj) [ foo; x ]);
  assert (
    Obj.has_field obj foo pc
    = ite (eq foo x) (value_bool true) (value_bool true) );
  assert (Obj.has_field obj x pc = value_bool true);
  assert (
    Obj.has_field obj banana pc
    = ite (eq banana x) (value_bool true) (value_bool false) );
  assert (
    Obj.has_field obj y pc
    = ite (eq y x) (value_bool true)
        (ite (eq y foo) (value_bool true) (value_bool false)) );
  assert (Obj.get obj foo pc = [ (ite (eq foo x) val_200 val_100, pc) ]);
  assert (Obj.get obj x pc = [ (val_200, pc) ]);
  (* [get "banana"] If "banana" = x then 200 else undef *)
  assert (Obj.get obj banana pc = [ (ite (eq banana x) val_200 undef, pc) ]);
  (* [get y] If y = "foo" then ite(x = "foo", "bar", 100) else if y = x then "bar" else undef *)
  assert (
    Obj.get obj y pc
    = [ (ite (eq y x) val_200 (ite (eq y foo) val_100 undef), pc) ] );

  (*********** Concrete write {"banana" : 300} -> {"banana" : 300} ; {"foo": 100; x : 200} ***********)
  let obj, pc = get_obj (Obj.set obj ~field:banana ~data:val_300 pc) in
  assert (
    Obj.has_field obj foo pc
    = ite (eq foo x) (value_bool true) (value_bool true) );
  assert (Obj.has_field obj banana pc = value_bool true);
  (* FIXME: To check: [has_field x] Será que isto faz sentido? deverá ter o caso do x=foo? *)
  assert (
    Obj.has_field obj x pc
    = ite (eq x banana) (value_bool true) (value_bool true) );
  (* [has_field y] If y = banana then true else if y = x then true else if y = foo then true else false *)
  assert (
    Obj.has_field obj y pc
    = ite (eq y banana) (value_bool true)
        (ite (eq y x) (value_bool true)
           (ite (eq y foo) (value_bool true) (value_bool false)) ) );
  assert (Obj.get obj foo pc = [ (ite (eq foo x) val_200 val_100, pc) ]);
  assert (Obj.get obj banana pc = [ (val_300, pc) ]);
  (* [get x] If x = banana then 300 else 200 *)
  assert (Obj.get obj x pc = [ (ite (eq x banana) val_300 val_200, pc) ]);
  (* [get y] if y = foo then ite(x = "foo", 200, 100) else if y = banana then 300 else if y = x then 200 else undef *)
  assert (
    Obj.get obj y pc
    = [ ( ite (eq y banana) val_300
            (ite (eq y x) val_200 (ite (eq y foo) val_100 undef))
        , pc )
      ] );

  (*********** Symbolic write {y : 400} ***********)
  (* {"banana" : 300}; y : 400 ;
     {"foo": 100; x : 200} *)
  let obj, pc = get_obj (Obj.set obj ~field:y ~data:val_400 pc) in
  assert (list_is_equal (Obj.get_fields obj) [ foo; x; banana; y ]);
  assert (
    Obj.get obj foo pc
    = [ (ite (eq foo y) val_400 (ite (eq foo x) val_200 val_100), pc) ] );
  assert (
    Obj.get obj x pc
    = [ (ite (eq x y) val_400 (ite (eq x banana) val_300 val_200), pc) ] );
  assert (Obj.get obj banana pc = [ (ite (eq banana y) val_400 val_300, pc) ]);
  assert (Obj.get obj y pc = [ (val_400, pc) ])
