open Utils
open Memory_models
module Obj = Object_mwl.M

let x = key_s "x"
let _y = key_s "y"
let _z = key_s "z"
let foo = key_c "foo"
let _banana = key_c "banana"
let age = key_c "age"
let bar = key_c "bar"
let val_10 = value_int 10
let val_100 = value_int 100
let val_200 = value_int 200
let val_300 = value_int 300
let _val_400 = value_int 400
let _val_500 = value_int 500

(* Test case 4.2: Delete after two instances of the same field in different records pc = (foo = x)*)
let () =
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
  let has_field = Obj.has_field obj foo pc in
  Format.printf "has_field foo: %a\n" Encoding.Expr.pp has_field;
  Format.printf "expected  foo: true\n";
  (* assert (Obj.has_field obj foo pc = value_bool true) *)
