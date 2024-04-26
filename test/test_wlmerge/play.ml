open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

let () =
  let x = key_s "x" in
  let y = key_s_int "y" in
  let z = key_s "z" in
  let a = key_c "a" in
  let c = key_c "c" in
  let _d = key_c "d" in
  let val_3 = value_int 3 in
  let val_4 = value_int 4 in
  let val_5 = value_int 5 in
  let _val_6 = value_int 6 in
  let pc = value_bool true in
  let cond = gt y (value_int 3) in
  (*
     o = {};            
   o.a = 3;
   o2[#x] = 4;             // o : {{a:3};_;0} || o2 : {{};#x:4;0}
  ------------time: 0-------> {content: [o, o2], time: 0}
  if (#y > 3) {
    ----------time: 1-------> {content: [None, None]; time: 1}; {content: [{{a:3};_;0} || {{};#x:4;0}], time: 0}
     o[#z] = 3;            // o : {{};_;1}; {{};#z: 3;1}; {{a:3};_;0}
    ----------time: 1-------> {content: [ {{};_;1}; {{};#z: 3;1}; {{a:3};_;0} ||  None]; time: 1}; {content: [{{a:3};_;0} || {{};#x:4;0}], time: 0}
   } else {
    ----------time: 2-------> {content: [None, None]; time: 2}; {content: [{{a:3};_;0} || {{};#x:4;0}], time: 0}
     o.c = 5;              // o : {{c:5};_;2}; {{a:3};_;0}
    ----------time: 2-------> {content: [ {{c:5};_;2}; {{a:3};_;0} || None]; time: 2}; {content: [{{a:3};_;0} || {{};#x:4;0}], time: 0}
   }
  ------------time: 3 ------> {content: [ {{};_;3}; If{3;#y > 3 then {{};_;1}; {{};#z: 3;1} else {{c:5};_;2}} 
                                       || {{};#x:4;0}], time: 3}
   o.d = 6
   -------------------------> {content: [ {{d:6};_;3}; If{3;#y > 3 then {{};_;1}; {{};#z: 3;1} else {{c:5};_;2}} 
                                       || {{};#x:4;0}], time: 3}

   o.a --> [#y > 3 && #z = a, 3]; [#x = a, 4]; [True, 3]  -> ITE(#y > 3 && #z = a, 3, ITE(#x = a, 4, 3))
  *)
  let mem = Mem.create () in
  let loc = Mem.alloc mem in
  let loc2 = Mem.alloc mem in
  let _ = Mem.set mem loc ~field:a ~data:val_3 pc in
  let _ = Mem.set mem loc2 ~field:x ~data:val_4 pc in
  let then_mem = Mem.clone mem in
  let else_mem = Mem.clone mem in
  let _ = Mem.set then_mem loc ~field:z ~data:val_3 pc in
  Format.printf "----Memory then_mem: ----\n%a\n\n\n" Mem.pp then_mem;
  let _ = Mem.set else_mem loc ~field:c ~data:val_5 pc in
  Format.printf "----Memory else_mem: ----\n%a\n\n\n" Mem.pp else_mem;
  let merged_mem = Mem.merge then_mem else_mem cond in
  Format.printf "----Memory merged_mem: ----\n%a\n" Mem.pp merged_mem;
  ()
