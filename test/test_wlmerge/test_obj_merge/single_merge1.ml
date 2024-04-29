open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

let () =
  let x = key_s_int "x" in
  let p = key_c "p" in
  let pc = value_bool true in
  let cond = gt x (value_int 0) in

  (*
  o := {}; 
    if (#x > 0) {
      o.p := #x; 
      o = R1{{p: #x};_;1}:{{};_;0}
   } else {}
   
    singleMerge(R1{{p: #x};_;1}:{{};_;0}, 0, #x > 0) = ITE(#x > 0, [R1{{p: #x};_;1, [])]; {{};_;0}
  *)
  let obj = Obj.create () in

  let then_obj = Obj.clone obj 1 in
  let _else_obj = Obj.clone obj 2 in

  let then_obj, _pc = get_obj (Obj.set then_obj ~field:p ~data:x pc) in

  let merged_obj = Obj.single_merge then_obj 0 cond in

  (* print_obj Obj.pp merged_obj; *)
  assert (Obj.get merged_obj p pc = [ (ite cond x undef, pc) ])
