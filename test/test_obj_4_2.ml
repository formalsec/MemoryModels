open Utils
open Memory_models
module Obj = Object_mwl.M

(* Test case 4.2: pc = ("foo" = x)
   Delete after two instances of the same field in different records pc = (foo = x)*)
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
  let pc = eq foo x in

  (*********** Create object {"foo": 100； "bar": 200; "age": 10} ***********)
  let obj = Obj.create () in
  let obj, pc = get_obj (Obj.set obj ~field:foo ~data:val_100 pc) in
  let obj, pc = get_obj (Obj.set obj ~field:bar ~data:val_200 pc) in
  let obj, pc = get_obj (Obj.set obj ~field:age ~data:val_10 pc) in

  assert (
    list_is_equal (Obj.to_list obj)
      [ (foo, val_100); (bar, val_200); (age, val_10) ] );
  assert (list_is_equal (Obj.get_fields obj) [ foo; bar; age ]);

  (*********** Symbolic write {x : 300} -> {} ; {"foo": 100； "bar": 200; "age": 10; x : 300}  ***********)
  let obj, pc = get_obj (Obj.set obj ~field:x ~data:val_300 pc) in
  assert (Obj.has_field obj foo pc = value_bool true);
  assert (Obj.has_field obj bar pc = value_bool true);
  assert (Obj.has_field obj age pc = value_bool true);
  assert (Obj.has_field obj x pc = value_bool true);
  assert (Obj.has_field obj banana pc = value_bool false);
  assert (
    Obj.has_field obj z pc
    = ite (eq z x) (value_bool true)
        (ite (eq z bar) (value_bool true)
           (ite (eq z age) (value_bool true) (value_bool false)) ) );

  assert (Obj.get obj foo pc = [ (val_300, pc) ]);
  assert (Obj.get obj bar pc = [ (val_200, pc) ]);
  assert (Obj.get obj age pc = [ (val_10, pc) ]);
  assert (Obj.get obj x pc = [ (val_300, pc) ]);
  assert (Obj.get obj banana pc = [ (undef, pc) ]);
  assert (
    Obj.get obj z pc
    = [ ( ite (eq z x) val_300
            (ite (eq z bar) val_200 (ite (eq z age) val_10 undef))
        , pc )
      ] );

  (*********** Concrete write {"foo" : 400} ***********)
  (* {"foo": 400} ;
     {"foo": 100; "bar": 200; "age": 10; x : 300} *)
  let obj, pc = get_obj (Obj.set obj ~field:foo ~data:val_400 pc) in

  assert (Obj.has_field obj foo pc = value_bool true);
  assert (Obj.has_field obj bar pc = value_bool true);
  assert (Obj.has_field obj age pc = value_bool true);
  assert (Obj.has_field obj x pc = value_bool true);
  assert (Obj.has_field obj banana pc = value_bool false);
  assert (
    Obj.has_field obj z pc
    = ite (eq z foo) (value_bool true)
        (ite (eq z bar) (value_bool true)
           (ite (eq z age) (value_bool true) (value_bool false)) ) );

  assert (Obj.get obj foo pc = [ (val_400, pc) ]);
  assert (Obj.get obj bar pc = [ (val_200, pc) ]);
  assert (Obj.get obj age pc = [ (val_10, pc) ]);
  assert (Obj.get obj x pc = [ (val_400, pc) ]);
  assert (
    Obj.get obj z pc
    = [ ( ite (eq z foo) val_400
            (ite (eq z bar) val_200 (ite (eq z age) val_10 undef))
        , pc )
      ] );

  (*********** Symbolic write {y : 500} ***********)
  (* {}
     {"foo": 400; y : 500} ;
     {"foo": 100; "bar": 200; "age": 10; x : 300} *)
  let obj, pc = get_obj (Obj.set obj ~field:y ~data:val_500 pc) in

  assert (
    Obj.has_field obj foo pc
    = ite (eq foo y) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj bar pc
    = ite (eq bar y) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj age pc
    = ite (eq age y) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj x pc = ite (eq x y) (value_bool true) (value_bool true) );
  assert (Obj.has_field obj y pc = value_bool true);
  assert (
    Obj.has_field obj banana pc
    = ite (eq banana y) (value_bool true) (value_bool false) );
  assert (
    Obj.has_field obj z pc
    = ite (eq z y) (value_bool true)
        (ite (eq z foo) (value_bool true)
           (ite (eq z bar) (value_bool true)
              (ite (eq z age) (value_bool true) (value_bool false)) ) ) );

  assert (Obj.get obj foo pc = [ (ite (eq foo y) val_500 val_400, pc) ]);
  assert (Obj.get obj bar pc = [ (ite (eq bar y) val_500 val_200, pc) ]);
  assert (Obj.get obj age pc = [ (ite (eq age y) val_500 val_10, pc) ]);
  assert (Obj.get obj x pc = [ (ite (eq x y) val_500 val_400, pc) ]);
  assert (Obj.get obj y pc = [ (val_500, pc) ]);
  assert (Obj.get obj banana pc = [ (ite (eq banana y) val_500 undef, pc) ]);
  assert (
    Obj.get obj z pc
    = [ ( ite (eq z y) val_500
            (ite (eq z foo) val_400
               (ite (eq z bar) val_200 (ite (eq z age) val_10 undef)) )
        , pc )
      ] );

  (*********** Delete foo ***********)
  (* {"foo": None}
     {"foo": 400; y : 500} ;
     {"foo": 100; "bar": 200; "age": 10; x : 300} *)
  let obj, pc = get_obj (Obj.delete obj foo pc) in

  assert (Obj.has_field obj foo pc = value_bool false);
  assert (
    Obj.has_field obj bar pc
    = ite (eq bar y) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj age pc
    = ite (eq age y) (value_bool true) (value_bool true) );
  assert (Obj.has_field obj x pc = value_bool false);
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

  assert (Obj.get obj foo pc = [ (undef, pc) ]);
  assert (Obj.get obj bar pc = [ (ite (eq bar y) val_500 val_200, pc) ]);
  assert (Obj.get obj age pc = [ (ite (eq age y) val_500 val_10, pc) ]);
  assert (Obj.get obj x pc = [ (undef, pc) ]);
  assert (Obj.get obj y pc = [ (ite (eq y foo) undef val_500, pc) ]);
  assert (
    Obj.get obj z pc
    = [ ( ite (eq z foo) undef
            (ite (eq z y) val_500
               (ite (eq z bar) val_200 (ite (eq z age) val_10 undef)) )
        , pc )
      ] );

  (*********** Symbolic write {x : 300} ***********)
  (* {}
     {"foo": None; x : 300}
     {"foo": 400; y : 500} ;
     {"foo": 100; "bar": 200; "age": 10; x : 300} *)
  let obj, pc = get_obj (Obj.set obj ~field:x ~data:val_300 pc) in

  assert (Obj.has_field obj foo pc = value_bool true);
  assert (
    Obj.has_field obj bar pc
    = ite (eq bar y) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj age pc
    = ite (eq age y) (value_bool true) (value_bool true) );
  assert (Obj.has_field obj x pc = value_bool true);
  assert (
    Obj.has_field obj y pc = ite (eq y x) (value_bool true) (value_bool true) );
  assert (
    Obj.has_field obj banana pc
    = ite (eq banana y) (value_bool true) (value_bool false) );
  assert (
    Obj.has_field obj z pc
    = ite (eq z x) (value_bool true)
        (ite (eq z y) (value_bool true)
           (ite (eq z bar) (value_bool true)
              (ite (eq z age) (value_bool true) (value_bool false)) ) ) );

  assert (Obj.get obj foo pc = [ (val_300, pc) ]);
  assert (Obj.get obj bar pc = [ (ite (eq bar y) val_500 val_200, pc) ]);
  assert (Obj.get obj age pc = [ (ite (eq age y) val_500 val_10, pc) ]);
  assert (Obj.get obj x pc = [ (val_300, pc) ]);
  assert (Obj.get obj y pc = [ (ite (eq y x) val_300 val_500, pc) ]);
  assert (
    Obj.get obj z pc
    = [ ( ite (eq z x) val_300
            (ite (eq z y) val_500
               (ite (eq z bar) val_200 (ite (eq z age) val_10 undef)) )
        , pc )
      ] )
