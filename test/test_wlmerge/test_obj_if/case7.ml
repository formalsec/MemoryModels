open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M

(* Test case:
   test if - case 7 *)
let () =
  let a = key_c "a" in
  let b = key_c "b" in
  let c = key_c "c" in
  let x = key_s "x" in
  let z = key_s "z" in
  let y = key_s_int "y" in
  let val_1 = value_int 1 in
  let val_2 = value_int 2 in
  let val_3 = value_int 3 in
  let pc = value_bool true in
  let cond = gt y (value_int 0) in

  (*
      o = {}; 
      o.a = 3; 
      if (#y > 0) {
        o.b = 1;
      } else {
        o[#z]= 2; 
      }

  *)
  let obj = Obj.create () in

  let obj, pc = get_obj (Obj.set obj ~field:a ~data:val_3 pc) in

  let then_obj = Obj.clone obj in
  let else_obj = Obj.clone obj in

  let then_obj, pc = get_obj (Obj.set then_obj ~field:b ~data:val_1 pc) in

  let else_obj, _pc = get_obj (Obj.set else_obj ~field:z ~data:val_2 pc) in

  let merged_obj = Obj.merge then_obj else_obj cond in

  (* test get *)
  assert (
    Obj.get merged_obj a pc
    = [ (ite (and_ (eq a z) (not_ cond)) val_2 val_3, pc) ] );
  assert (
    Obj.get merged_obj b pc
    = [ (ite cond val_1 (ite (and_ (eq b z) (not_ cond)) val_2 undef), pc) ] );
  assert (
    Obj.get merged_obj c pc
    = [ (ite (and_ (eq c z) (not_ cond)) val_2 undef, pc) ] );
  assert (
    Obj.get merged_obj z pc
    = [ ( ite
            (and_ (eq z b) cond)
            val_1
            (ite (not_ cond) val_2 (ite (eq z a) val_3 undef))
        , pc )
      ] );
  assert (
    Obj.get merged_obj x pc
    = [ ( ite
            (and_ (eq x b) cond)
            val_1
            (ite (and_ (eq x z) (not_ cond)) val_2 (ite (eq x a) val_3 undef))
        , pc )
      ] )
