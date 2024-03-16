open Utils
open Memory_models
module Obj = Object_symbolic.M

(* Test case 3: Object with 1 concrete field followed by 1 symbolic field *)
let () =
  let x = key_s "x" in
  let y = key_s "y" in
  let foo = key_c "foo" in
  let banana = key_c "banana" in
  let val_bar = value_str "bar" in
  let _val_apple = value_str "apple" in
  let val_one = value_str "one" in
  let _val_two = value_str "two" in

  (*********** Create object {x: "one"} ***********)
  let obj = Obj.create () in
  let obj = Obj.set obj ~key:x ~data:val_one in
  assert (list_is_equal (Obj.to_list obj) [ (x, val_one) ]);
  assert (list_is_equal (Obj.get_fields obj) [ x ]);

  (*********** Concrete write {foo : "bar"} -> {"foo": "bar" ; x : "one"} ***********)
  let obj = Obj.set obj ~key:foo ~data:val_bar in
  assert (list_is_equal (Obj.to_list obj) [ (x, val_one); (foo, val_bar) ]);
  assert (list_is_equal (Obj.get_fields obj) [ x; foo ]);
  (* FIXME: (3) [get] when receiving a symbolic field that exists in object, it just return what it has stored.
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
      ] )
