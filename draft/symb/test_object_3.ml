open Utils
open Memory_models
module Obj = Object_symbolic.M

(* Test case 3: Object with 1 symbolic field followed by 1 concrete field *)
let () =
  let x = key_s "x" in
  let y = key_s "y" in
  let foo = key_c "foo" in
  let banana = key_c "banana" in
  let val_bar = value_str "bar" in
  let _val_apple = value_str "apple" in
  let val_one = value_str "one" in
  let val_two = value_str "two" in

  (*********** Create object {x: "one"} ***********)
  let obj = Obj.create () in
  let obj = Obj.set obj ~key:x ~data:val_one in
  assert (list_is_equal (Obj.to_list obj) [ (x, val_one) ]);
  assert (list_is_equal (Obj.get_fields obj) [ x ]);

  (*********** Concrete write {foo : "bar"} -> {"foo": "bar" ; x : "one"} ***********)
  let obj = Obj.set obj ~key:foo ~data:val_bar in
  assert (list_is_equal (Obj.to_list obj) [ (x, val_one); (foo, val_bar) ]);
  assert (list_is_equal (Obj.get_fields obj) [ x; foo ]);
  assert (Obj.has_field obj foo = value_bool true);
  assert (
    Obj.has_field obj x
    = ite (eq x foo) (value_bool true)
        (ite (eq x x) (value_bool true) (value_bool false)) );
  (* FIXME: (1) *)
  (* Format.printf "[has_field banana]: %a\n\n" Encoding.Expr.pp (Obj.has_field obj banana); *)
  assert (
    Obj.has_field obj banana
    = ite (eq banana x) (value_bool true) (value_bool false) );
  assert (
    Obj.has_field obj y
    = ite (eq y foo) (value_bool true)
        (ite (eq y x) (value_bool true) (value_bool false)) );
  (* FIXME: (4) [get] when receiving a symbolic field that exists in object, it just return what it has stored.
     In this case the problem is when x = "foo". So, at the beginning, we have "foo": "one". Then, by doing a concrete write of "foo",
     we would have an object with "foo" : "bar". Therefore, when we want to get the value of the symbolic field x, it should also
     search in the concrete table OR in someway, when settinhg this concrete field "foo" should also modify the symbolic fields (we need to
     think better about the solution) *)
  (* [get x] If x = "foo" then "bar" else "one" *)
  (* Format.printf "[get x]: %a\n\n" (Fmt.pp_lst (fun fmt (a, b) -> Fmt.fprintf fmt "(%a, %a)" Expr.pp a (Fmt.pp_lst Expr.pp) b)) (Obj.get obj x); *)
  assert (
    Obj.get obj x = [ (val_bar, [ eq x foo ]); (val_one, [ ne (eq x foo) ]) ] );
  assert (Obj.get obj foo = [ (val_bar, []) ]);
  assert (
    list_is_equal (Obj.get obj banana)
      [ (val_one, [ eq banana x ]); (undef, [ ne (eq banana x) ]) ] );

  (* FIXME: (2) [get] when receiving a symbolic field that does not exists in object, just take into consideration the concrete table,
      does not take into consideration the symbolic fields that the object already has. After fix, note that the conditions is a list,
     so it might not be in this order *)
  (* [get y] If y = "foo" then "bar" else if y = x then "one" else undef *)
  (* Format.printf "[get y]: \n%a\n\n" (Fmt.pp_lst (fun fmt (a, b) -> Fmt.fprintf fmt "(%a, %a)" Expr.pp a (Fmt.pp_lst Expr.pp) b)) (Obj.get obj y); *)
  assert (
    list_is_equal (Obj.get obj y)
      [ (val_bar, [ eq y foo ])
      ; (val_one, [ ne (eq y foo); eq y x ])
      ; (undef, [ ne (eq y foo); ne (eq y x) ])
      ] );

  (*********** Symbolic write {y : "two"} ***********)
  (* {
     "foo" : ite(y="foo", "two", "bar") ;
       x   : ite(y=x, "two", "one");
       y   : "two"} *)
  let obj = Obj.set obj ~key:y ~data:val_two in
  assert (
    list_is_equal (Obj.to_list obj)
      [ (x, ite (eq y x) val_two val_one)
      ; (foo, ite (eq y foo) val_two val_bar)
      ; (y, val_two)
      ] );
  assert (list_is_equal (Obj.get_fields obj) [ x; foo; y ]);
  assert (Obj.has_field obj foo = value_bool true);
  assert (
    Obj.has_field obj x
    = ite (eq x foo) (value_bool true)
        (ite (eq x x) (value_bool true)
           (ite (eq x y) (value_bool true) (value_bool false)) ) );
  (* FIXME: (1) *)
  (* Format.printf "[has_field banana]: %a\n\n" Encoding.Expr.pp (Obj.has_field obj banana); *)
  assert (
    Obj.has_field obj banana
    = ite (eq banana x) (value_bool true)
        (ite (eq banana y) (value_bool true) (value_bool false)) );
  assert (
    Obj.has_field obj y
    = ite (eq y foo) (value_bool true)
        (ite (eq y x) (value_bool true)
           (ite (eq y y) (value_bool true) (value_bool false)) ) );
  assert (Obj.get obj foo = [ (ite (eq y foo) val_two val_bar, []) ]);
  (* FIXME: (4) *)
  (* Format.printf "get x: %a\n\n" (Fmt.pp_lst (fun fmt (a, b) -> Fmt.fprintf fmt "(%a, %a)" Encoding.Expr.pp a (Fmt.pp_lst Encoding.Expr.pp) b)) (Obj.get obj x); *)
  (* [get x] If x = "foo" then ite(y="foo", "two", "bar") else "one" *)
  assert (
    list_is_equal (Obj.get obj x)
      [ (ite (eq y foo) val_two val_bar, [ eq x foo ])
      ; (val_one, [ ne (eq x foo) ])
      ] );
  (* [get banana] If banana = x then "one" else if banana = y then "two" else undef *)
  assert (
    list_is_equal (Obj.get obj banana)
      [ (ite (eq banana y) val_two (ite (eq y x) val_two val_one), [])
      ; (undef, [ ne (eq banana y); ne (eq banana x) ])
      ] );
  (* [get y] "two" *)
  assert (Obj.get obj y = [ (val_two, []) ])
