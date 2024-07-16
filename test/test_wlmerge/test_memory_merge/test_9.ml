open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test memory merge when the object is created inside both branches *)
let () =
  let y = key_s_int "y" in
  let x = key_s_int "x" in
  let z = key_s "z" in
  let a = key_c "a" in
  let c = key_c "c" in
  let val_3 = value_int 3 in
  let val_5 = value_int 5 in
  let val_200 = value_int 200 in
  let pc = gt x (value_int 4) in
  let cond = gt y (value_int 3) in

  (*
    pc = x > 4

    if (#y > 3) {
      o = {};            
      o.c = 3;            
      o.a = 200;
    } else {
      o = {};            
      o.c = 5;              
    }

    o.c
    o.2
  *)
  let mem = Mem.create () in

  (* if *)
  let loc = Mem.alloc mem in
  let _ = Mem.set mem loc ~field:c ~data:val_3 pc in
  let _ = Mem.set mem loc ~field:a ~data:val_200 pc in

  (* else *)
  let loc2 = Mem.alloc mem in
  let _ = Mem.set mem loc2 ~field:c ~data:val_5 pc in

  (* merge -> ite(cond, loc, loc2) *)
  let merged_loc = ite cond loc loc2 in

  assert (
    Mem.get mem merged_loc c pc
    = [ (ite cond val_3 val_5, pc) ] );
  assert (
    Mem.get mem merged_loc a pc
    = [ (ite cond val_200 undef, pc) ] );

  assert (
    Mem.has_field mem merged_loc c pc
    = ite cond (value_bool true) (ite (not_ cond) (value_bool true) (value_bool false)));
  assert (
    Mem.has_field mem merged_loc a pc
    = ite cond (value_bool true) (ite (not_ cond) (value_bool false) (value_bool false)));
  
    (*
     ite(#y>3,
           then_expr,
           else_expr)

     then_expr = ite(z=c,
                       3,
                       ite(z=a,
                             200,
                             undef))
     else_expr = ite(z=c,
                       5,
                       undef)
  *)
  assert (
    Mem.get mem merged_loc z pc
    = [ ( ite cond
            (ite (eq z c) val_3 (ite (eq z a) val_200 undef))
            (ite (eq z c) val_5 undef)
        , pc )
      ] );
  assert (
    Mem.has_field mem merged_loc z pc
    = ite cond
            (ite (eq z c) (value_bool true) (ite (eq z a) (value_bool true) (value_bool false)))
            (ite (not_ cond) (ite (eq z c) (value_bool true) (value_bool false)) (value_bool false))
        )

