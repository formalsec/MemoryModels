open Test.Utils
open Memory_models
module Expr = Smtml.Expr
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test memory merge, assign different location of the object in the branches.
   After merge, do a write operation. *)
let () =
  let x = key_s "x" in
  let y = key_s_int "y" in
  let foo = key_c "foo" in
  let _b = key_c "b" in
  let _val_0 = value_int 0 in
  let _val_1 = value_int 1 in
  let _val_2 = value_int 2 in
  let val_3 = value_int 3 in
  let _val_4 = value_int 4 in
  let pc = value_bool true in
  let cond = gt y (value_int 0) in

  (*
    o1 = {};
    o2 = {};
    --------------------> lo1 = {[]; -}; lo2 = {[]; -}
    if(#y > 0){
      o = o1; 
    } else {
      o = o2; 
    }
    --------------------> lo1 = {[]; -}; lo2 = {[]; -}
    o.foo = 3; 
    --------------------> lo1 = {[]; -}; if(#y > 0){[foo->3]; -} else {}; {[]; -}
    --------------------> lo2 = {[]; -}; if(#y > 0){[foo->3]; -} else {}; {[]; -}
  *)
  let mem = Mem.create () in
  let loc1 = Mem.alloc mem in
  let loc2 = Mem.alloc mem in

  (* merge -> o = ite(#y > 0, loc1, loc2) *)
  let loc = ite cond loc1 loc2 in

  let _ = Mem.set mem loc ~field:foo ~data:val_3 pc in

  assert (
    Mem.get mem loc foo pc
    = [ (ite cond val_3 (ite (not_ cond) val_3 undef), pc) ] );
  assert (
    Mem.get mem loc x pc
    = [ ( ite cond
            (ite (eq x foo) val_3 undef)
            (ite (not_ cond) (ite (eq x foo) val_3 undef) undef)
        , pc )
      ] );

  (* pc = #y > 0 *)
  assert (Mem.get mem loc foo cond = [ (val_3, cond) ]);
  assert (Mem.get mem loc x cond = [ (ite (eq x foo) val_3 undef, cond) ]);

  (* pc = !(#y > 0) *)
  assert (Mem.get mem loc foo (not_ cond) = [ (val_3, not_ cond) ]);
  assert (
    Mem.get mem loc x (not_ cond) = [ (ite (eq x foo) val_3 undef, not_ cond) ] )
