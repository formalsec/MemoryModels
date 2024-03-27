open Utils
open Memory_models
module Obj = Object_symbolic.M

(* Test1: Base cases tests *)
let () =
  let x = key_s "x" in
  let y = key_s "y" in
  let foo = key_c "foo" in
  let banana = key_c "banana" in

  (*********** Create an empty object ***********)
  let obj = Obj.create () in
  assert (Obj.is_empty obj);
  assert (Obj.to_list obj = []);
  assert (Obj.get_fields obj = []);
  assert (Obj.has_field obj foo = value_bool false);
  assert (Obj.has_field obj x = value_bool false);
  assert (Obj.get obj foo = []);
  assert (Obj.get obj x = []);
  assert (Obj.delete obj foo = obj);

  (*********** Concrete write {"foo": 100} ***********)
  let val_100 = value_int 100 in
  let obj = Obj.set obj ~key:foo ~data:val_100 in
  assert (not (Obj.is_empty obj));
  assert (Obj.to_list obj = [ (foo, val_100) ]);
  assert (Obj.get_fields obj = [ foo ]);
  assert (Obj.has_field obj foo = value_bool true);
  assert (Obj.has_field obj banana = value_bool false);
  (* [has_field x] If x = foo then true else false *)
  assert (
    Obj.has_field obj x = ite (eq x foo) (value_bool true) (value_bool false) );
  assert (Obj.get obj foo = [ (val_100, []) ]);
  assert (Obj.get obj banana = []);
  (* [get x] If x = foo then val_100, else undefined *)
  assert (
    list_is_equal (Obj.get obj x)
      [ (val_100, [ eq x foo ]); (undef, [ ne (eq x foo) ]) ] );

  (*********** Delete field foo -> Empty object ***********)
  let obj = Obj.delete obj foo in
  assert (Obj.is_empty obj);
  assert (Obj.to_list obj = []);
  assert (Obj.get_fields obj = []);
  assert (Obj.has_field obj foo = value_bool false);
  assert (Obj.has_field obj x = value_bool false);
  assert (Obj.get obj foo = []);
  assert (Obj.get obj x = []);
  assert (Obj.delete obj foo = obj);

  (*********** Symbolic write {x : 200} ***********)
  let val_200 = value_int 200 in
  let obj = Obj.set obj ~key:x ~data:val_200 in
  assert (not (Obj.is_empty obj));
  assert (Obj.to_list obj = [ (x, val_200) ]);
  assert (Obj.get_fields obj = [ x ]);
  (* FIXME: (1) [has_field] when receiving a concrete field just search in the concrete table, does not take consideration
     symbolic fields that are stored in the object *)
  (* Format.printf "Has field foo: %a\n\n" Expr.pp (Obj.has_field obj foo); *)
  (* [has_field "foo"] If "foo" = x then true else false *)
  assert (
    Obj.has_field obj foo = ite (eq foo x) (value_bool true) (value_bool false) );
  (* [has_field x] If x = x then true else false *)
  assert (
    Obj.has_field obj x = ite (eq x x) (value_bool true) (value_bool false) );
  (* [has_field y] If y = x then true else false *)
  assert (
    Obj.has_field obj y = ite (eq y x) (value_bool true) (value_bool false) );
  assert (Obj.get obj x = [ (val_200, []) ]);
  (* [get foo] If foo = x then 200 else undefined *)
  assert (
    list_is_equal (Obj.get obj foo)
      [ (val_200, [ eq foo x ]); (undef, [ ne (eq foo x) ]) ] )
