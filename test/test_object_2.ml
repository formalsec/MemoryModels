open Utils
open Memory_models
module Obj = Object_symbolic.M

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

  (*********** Create object {"foo": 100} ***********)
  let obj = Obj.create () in
  let obj = Obj.set obj ~key:foo ~data:val_100 in
  assert (list_is_equal (Obj.to_list obj) [ (foo, val_100) ]);
  assert (list_is_equal (Obj.get_fields obj) [ foo ]);

  (*********** Symbolic write {x : 200} -> {"foo": ite(x = "foo", 200, 100); x : 200} ***********)
  let obj = Obj.set obj ~key:x ~data:val_200 in
  assert (
    list_is_equal (Obj.to_list obj)
      [ (foo, ite (eq x foo) val_200 val_100); (x, val_200) ] );
  assert (list_is_equal (Obj.get_fields obj) [ foo; x ]);
  assert (Obj.has_field obj foo = value_bool true);
  assert (Obj.has_field obj x =  ite (eq x foo) (value_bool true) (ite (eq x x) (value_bool true) (value_bool false)));
  assert (Obj.get obj foo = [ (ite (eq x foo) val_200 val_100, []) ]);
  assert (Obj.get obj x = [ (val_200, []) ]);
  (* [get "banana"] If "banana" = x then 200 else undef *)
  assert (
    list_is_equal (Obj.get obj banana)
      [ (val_200, [ eq banana x ]); (undef, [ ne (eq banana x) ]) ] );

  (* FIXME: (2) [get] when receiving a symbolic field that does not exists in object, just take into consideration the concrete table,
      does not take into consideration the symbolic fields that the object already has. After fix, note that the conditions is a list,
     so it might not be in this order *)
  (* [get y] If y = "foo" then ite(x = "foo", "bar", 100) else if y = x then "bar" else undef *)
  (* Format.printf "[get y]: %a\n\n" (Fmt.pp_lst (fun fmt (a, b) -> Fmt.fprintf fmt "(%a, %a)" Expr.pp a (Fmt.pp_lst Expr.pp) b)) (Obj.get obj y); *)
  assert (
    list_is_equal (Obj.get obj y)
      [ (ite (eq x foo) val_200 val_100, [ eq y foo ])
      ; (val_200, [ ne (eq y foo); eq y x ])
      ; (undef, [ ne (eq y foo); ne (eq y x) ])
      ] );

  (*********** Concrete write {"banana" : 300} -> {"foo": ite(x = "foo", 200, 100); x : 200; "banana" : 300} ***********)
  let obj = Obj.set obj ~key:banana ~data:val_300 in
  assert (Obj.has_field obj foo = value_bool true);
  assert (Obj.has_field obj banana = value_bool true);
  (* [has_field x] If x = foo then true else if x = banana then true else if x = x then true else false *)
  assert (
    Obj.has_field obj x
    = ite (eq x foo) (value_bool true)
        (ite (eq x banana) (value_bool true)
           (ite (eq x x) (value_bool true) (value_bool false)) ) );
  assert (Obj.get obj foo = [ (ite (eq x foo) val_200 val_100, []) ]);
  assert (Obj.get obj banana = [ (val_300, []) ]);
  (* [get x] 200 *)
  assert (Obj.get obj x = [ (val_200, []) ]);

  (*********** Symbolic write {y : 400} ***********)
  (* {
       "foo"  : ite(y = "foo", 400, ite(x = "foo", 200, 100));
         x    : ite(y = x, 400, 200);
     "banana" : ite(y = "banana", 400, 300);
         y    : 400} *)
  let obj = Obj.set obj ~key:y ~data:val_400 in
  assert (list_is_equal (Obj.get_fields obj) [ foo; x; banana; y ]);
  assert (
    list_is_equal (Obj.to_list obj)
      [ (foo, ite (eq y foo) val_400 (ite (eq x foo) val_200 val_100))
      ; (x, ite (eq y x) val_400 val_200)
      ; (banana, ite (eq y banana) val_400 val_300)
      ; (y, val_400)
      ] );
  assert (
    Obj.get obj foo
    = [ (ite (eq y foo) val_400 (ite (eq x foo) val_200 val_100), []) ] );
  assert (Obj.get obj x = [ (ite (eq y x) val_400 val_200, []) ]);
  assert (Obj.get obj banana = [ (ite (eq y banana) val_400 val_300, []) ]);
  assert (Obj.get obj y = [ (val_400, []) ])
