open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

let () =
  let x = key_s "x" in
  let y = key_s_int "y" in
  let z = key_s "z" in
  let a = key_c "a" in
  let b = key_c "b" in
  let c = key_c "c" in
  let d = key_c "d" in
  let val_3 = value_int 3 in
  let val_4 = value_int 4 in
  let val_5 = value_int 5 in
  let val_6 = value_int 6 in
  let pc = value_bool true in
  let cond = gt y (value_int 3) in
  (*
  ---> time 0 
  o = {}          -> Heap.alloc  
  o.a = 3;        -> Heap.set
  o[#x] = 4       -> Heap.set 
  o2 = {}         -> Heap.alloc  
  ------------obj1---------> o = R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
                  [ { hp: [1 -> o]; { 1 }; 0} ]
                  -> Heap.clone -> [ { hp: []; {}; 1}; { hp: [1 -> o]; { 1 }; 0} ] 
                  -> Heap.clone -> [ { hp: []; {}; 2}; { hp: [1 -> o]; { 1 }; 0} ] 
   if (#y > 3) {
    o.b = 4;      -> Heap.set 
    o[#z] = 3;    -> Heap.set
   ------------obj2---------> o1 = R4{{};_;1};R3{{b: 4}; #z: 3; 1}; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
                  [ { hp: [1 -> o1]; { 1 }; 1}; { hp: [1 -> o]; { 1 }; 0} ]  
   } else {
    o.c = 5;      -> Heap.set
   ------------obj3---------> o2 = R5{{c:5};_;2}; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
                  [ { hp: [1 -> o2]; { 1 }; 2}; { hp: [1 -> o]; { 1 }; 0} ]  
   }
                  -> Heap.merge 
   o.d = 6
   ------------obj4---------> R6{{d:6};_}; [ (#y > 3) then R4{{};_;1};R3{{b: 4}; #z: 3; 1} else R5{{c:5};_;2} ]; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
   o.a --> [#y > 3 && #z = a, 3]; [#x = a, 4]; [True, 3]  -> ITE(#y > 3 && #z = a, 3, ITE(#x = a, 4, 3))
  *)
  let mem = Mem.create () in
  let loc = Mem.alloc mem in
  let _ = Mem.set mem loc ~field:a ~data:val_3 pc in
  let _ = Mem.set mem loc ~field:x ~data:val_4 pc in
  let _loc2 = Mem.alloc mem in

  let then_mem = Mem.clone mem 1 in
  let else_mem = Mem.clone mem 2 in

  let _ = Mem.set then_mem loc ~field:b ~data:val_4 pc in

  let _ = Mem.set then_mem loc ~field:z ~data:val_3 pc in
  Format.printf "----Memory then_mem: ----\n%a\n\n\n" Mem.pp then_mem;

  let _ = Mem.set else_mem loc ~field:c ~data:val_5 pc in
  Format.printf "----Memory else_mem: ----\n%a\n\n\n" Mem.pp else_mem;

  let mem = Mem.merge then_mem else_mem 0 cond in

  let _ = Mem.set mem loc ~field:d ~data:val_6 pc in

  Format.printf "Memory: %a@." Mem.pp mem;

  print_get a (Mem.get mem loc a pc)
