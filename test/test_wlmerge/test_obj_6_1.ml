open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M

(* Test case 6-1: pc = true
   Simple test of branching and merging
*)
let () =
  let x = key_s_int "x" in
  let s1 = key_s "s1" in
  let s2 = key_s "s2" in
  let p = key_c "p" in
  let q = key_c "q" in
  let val_3 = value_int 3 in
  let val_x_1 = plus x (value_int 1) in
  let val_4 = value_int 4 in
  let pc = value_bool true in
  let cond = gt x (value_int 0) in

  (*
  o := {};   
  o.p := 3;  
  ------------obj---------> R1{{p: 3};_; 0}
  if (#x > 0){ 
    o[#s1] := #x +1  
  ------------then_obj---------> R3{{};_; 1};R2{{}; #s1: #x+1; 1}; R1{{p: 3};_; 0}
  } else {
    o.p := o.p + 1; // R{{p: 4}, -, 2}; R{{p: 3}, -, 0}  
  ------------else_obj---------> R4{{p:4};_;2}; R1{{p: 3};_; 0}
  }
  ------------merged_obj---------> R5{{};_; 4}; If(#x > 0 then R3{{};_;1};R2{{}; #s1: #x+1; 1} else R4{{p:4};_;2}); R1{{p:3};_;0}  
  o.q = 4 
  ------------merged_obj---------> R5{{q:4};_; 4}; If(3;#x > 0 then R3{{};_;1};R2{{}; #s1: #x+1; 1} else R4{{p:4};_;2}); R1{{p:3};_;0}  
  FIXME: acho que está errado?
  o[#s2]  // ITE(#s2 = q, 4, ITE(#s2 = #s1, #x+1, ITE(#s2 = p, 3, undef)))
  o.q     // 4
  o.p     // ITE(#s1 = p, #x+1, 3)
*)
  let obj = Obj.create () in
  let obj, pc = get_obj (Obj.set obj ~field:p ~data:val_3 pc) in

  let then_obj = Obj.clone obj in
  let else_obj = Obj.clone obj in

  let then_obj, pc = get_obj (Obj.set then_obj ~field:s1 ~data:val_x_1 pc) in

  (* FIXME: The the operation is done in the interpreter side, so here just directly write int of 4
     o.p := o.p + 1; *)
  let else_obj, pc = get_obj (Obj.set else_obj ~field:p ~data:val_4 pc) in

  let merged_obj = Obj.merge then_obj else_obj cond in

  let merged_obj, _pc = get_obj (Obj.set merged_obj ~field:q ~data:val_4 pc) in

  (* test get *)
  assert (
    Obj.get merged_obj s2 pc
    = [ ( ite (eq s2 q) val_4
            (ite
               (and_ (eq s2 s1) cond)
               val_x_1
               (ite
                  (and_ (eq s2 p) (not_ cond))
                  val_4
                  (ite (eq s2 p) val_3 undef) ) )
        , pc )
      ] );
  assert (
    Obj.get merged_obj p pc
    = [ (ite (and_ (eq p s1) cond) val_x_1 (ite (not_ cond) val_4 val_3), pc) ] );

  assert (Obj.get merged_obj q pc = [ (val_4, pc) ])
