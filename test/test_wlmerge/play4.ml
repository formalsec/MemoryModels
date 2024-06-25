open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test memory merge
   Two object:
   - [o] was modified in the then branch
   - [o2] was modified in the else branch *)
let () =
  let x = key_s "x" in
  let y = key_s_int "y" in
  let z = key_s "z" in
  let a = key_c "a" in
  let b = key_c "b" in
  let d = key_c "d" in
  let val_3 = value_int 3 in
  let val_4 = value_int 4 in
  let val_6 = value_int 6 in
  let val_7 = value_int 7 in
  let pc = value_bool true in
  let cond = gt y (value_int 3) in

  (*
  ---> time 0 
  o = {}          -> Heap.alloc  0M = {0 -> loc_o};
  o.a = 3;        -> Heap.set    0M = {0 -> loc_o}; o = {{a:3};_; 0}
  o[#x] = 4       -> Heap.set    0M = {0 -> loc_o}; o = Empty;{{a:3}; #x:4 ; 0}
  o2 = {}         -> Heap.alloc  0M = {0 -> loc_o; 1 -> loc_o2}; o = {{a:3}; #x:4 ; 0}; o2 = {Empty0}
   if (#y > 3) {
    o2[#z] = 7;   -> Heap.set    1M = {1 -> loc_o2};{0 -> loc_o; 1 -> loc_o2}; o = Empty;{{a:3}; #x:4 ; 0}; o2 = Empty1;{{};#z:7;1};{Empty0}
    return;
   } else {
    o.b = 7;      -> Heap.set    1M = {0 -> loc_o};{0 -> loc_o; 1 -> loc_o2}; o = {{b:7}; _; 1};{{a:3}; #x:4 ; 0}; o2 =  {Empty0}
   }
                  -> Heap.single_merge on else branch and the pc here is !(#y > 3)
                    1M = {0 -> loc_o; 1 -> loc_o2}; 
                    o = {If #y > 3 then {{b:7}; _; 1} else Empty};{{a:3}; #x:4 ; 0}; 
                    o2 =  {Empty0}
   o.d = 6 

   -----> o  = {{d:6};_;0}; {If #y > 3 then {{b:7};_;1} else Empty}; {{a:3}; #x:4 ; 0}}
   -----> o2 = Empty(0)
  *)
  let mem = Mem.create () in
  let loc1 = Mem.alloc mem in
  let _ = Mem.set mem loc1 ~field:a ~data:val_3 pc in
  let _ = Mem.set mem loc1 ~field:x ~data:val_4 pc in
  let loc2 = Mem.alloc mem in

  let then_mem = Mem.clone mem 1 in
  let else_mem = Mem.clone mem 1 in

  let _ = Mem.set then_mem loc2 ~field:z ~data:val_7 pc in

  let _ = Mem.set else_mem loc1 ~field:b ~data:val_7 pc in

  let mem = Mem.single_merge else_mem 1 cond in


  let _ = Mem.set mem loc1 ~field:d ~data:val_6 pc in

  Format.printf "Memory: %a@." Mem.pp mem