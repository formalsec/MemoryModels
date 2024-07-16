open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test memory merge with nested if-then-else statements,
   assigning different location of the object in the branches.
   After merge, do a write operation. *)
let () =
  let x = key_s "x" in
  let y = key_s_int "y" in
  let c = key_c "c" in
  let val_3 = value_int 3 in
  let val_4 = value_int 4 in
  let val_5 = value_int 5 in
  let val_10 = value_int 10 in
  let pc = value_bool true in
  let cond1 = gt y (value_int 0) in
  let cond2 = lt y (value_int 3) in
  let y_eq_2 = eq y (value_int 2) in

  (*
    o1 = {};
    o2 = {};
    o3 = {};
    o4 = {};
    if(#y > 0){
        if (#y < 3){
          o4.c = 10;
          o = o1; 
        } else {
          o = o2; 
        }
    } else {
      o3.c = 4;
      o = o3; 
    }
    o.c = 3; 
    o3.c = 5;

  *)
  let mem = Mem.create () in
  let loc1 = Mem.alloc mem in
  let loc2 = Mem.alloc mem in
  let loc3 = Mem.alloc mem in
  let loc4 = Mem.alloc mem in

  let then_mem1 = Mem.clone mem 1 in
  let else_mem1 = Mem.clone mem 1 in

  (* then *)
  let then_mem2 = Mem.clone then_mem1 2 in
  let else_mem2 = Mem.clone then_mem1 2 in
  let _ = Mem.set then_mem2 loc4 ~field:c ~data:val_10 pc in
  let then_mem1 = Mem.merge then_mem2 else_mem2 2 cond2 in

  (* else *)
  let _ = Mem.set else_mem1 loc3 ~field:c ~data:val_4 pc in

  (* merge -> o = ite(#y > 0, ite(#y < 3, loc1, loc2), loc3) *)
  let loc = ite cond1 (ite cond2 loc1 loc2) loc3 in
  let mem = Mem.merge then_mem1 else_mem1 1 cond1 in

  let _ = Mem.set mem loc ~field:c ~data:val_3 pc in
  let _ = Mem.set mem loc3 ~field:c ~data:val_5 pc in

  assert (
    Mem.get mem loc c pc
    = [ ( ite (and_ cond1 cond2) val_3
            (ite (and_ cond1 (not_ cond2)) val_3 val_5)
        , pc )
      ] );
  assert (Mem.get mem loc1 c pc = [ (ite (and_ cond1 cond2) val_3 undef, pc) ]);

  let get_x_value = ite (eq x c) val_3 undef in
  assert (
    Mem.get mem loc x pc
    = [ ( ite (and_ cond1 cond2) get_x_value
            (ite
               (and_ cond1 (not_ cond2))
               get_x_value
               (ite (eq x c) val_5 undef) )
        , pc )
      ] );

  (* pc = (#y = 2) *)
  assert (Mem.get mem loc x y_eq_2 = [ (ite (eq x c) val_3 undef, y_eq_2) ]);
  assert (Mem.get mem loc3 x y_eq_2 = [ (ite (eq x c) val_5 undef, y_eq_2) ]);
  assert (Mem.get mem loc4 x y_eq_2 = [ (ite (eq x c) val_10 undef, y_eq_2) ]);

  assert (Mem.has_field mem loc x y_eq_2 = ite (eq x c) (value_bool true) (value_bool false));
  assert (Mem.has_field mem loc3 x y_eq_2 = ite (eq x c) (value_bool true) (value_bool false));
  assert (Mem.has_field mem loc4 x y_eq_2 = ite (eq x c) (value_bool true) (value_bool false))
