open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test memory merge with nested if-then-else statements with one object *)
let () =
  let x = key_s "x" in
  let y = key_s_int "y" in
  let a = key_c "a" in
  let b = key_c "b" in
  let val_0 = value_int 0 in
  let val_1 = value_int 1 in
  let val_2 = value_int 2 in
  let val_3 = value_int 3 in
  let val_4 = value_int 4 in
  let pc = value_bool true in
  let cond1 = gt y (value_int 0) in
  let cond2 = lt y (value_int 3) in

  (*
  o = {}          -> Heap.alloc  
  o.a = 0;        -> Heap.set
  o[#x] = 2;      -> Heap.set 
                  -> Heap.clone m
                  -> Heap.clone m
  if(#y > 0){    
    o.b = 1;      -> Heap.set
                  -> Heap.clone then_m 
                  -> Heap.clone then_m 
    if (#y < 3){  
      o.b = 2;    -> Heap.set
    } else {     
      o.b = 3;    -> Heap.set
    }            
                  -> Heap.merge 
  } else {
    o.b = 4;      -> Heap.set
  }
                  -> Heap.merge 
  ----> o = 
  Empty;
  {If (#y > 0) 
    then [Empty; 
          {If (#y < 3) then {{b:2};_} else {{b:3};_}};
          {{b:1};_};]
    else [{{b:4};_}]};
  Empty;
  {{a:0};#x:2;}
  *)
  let mem = Mem.create () in
  let loc1 = Mem.alloc mem in
  let _ = Mem.set mem loc1 ~field:a ~data:val_0 pc in
  let _ = Mem.set mem loc1 ~field:x ~data:val_4 pc in
  let _loc2 = Mem.alloc mem in

  let then_mem1, then_time1 = Mem.clone mem in
  let else_mem1, else_time1 = Mem.clone mem in

  let _ = Mem.set then_mem1 loc1 ~field:b ~data:val_1 pc in
  let then_mem2, then_time2 = Mem.clone then_mem1 in
  let else_mem2, else_time2 = Mem.clone then_mem1 in
  let _ = Mem.set then_mem2 loc1 ~field:b ~data:val_2 pc in
  let _ = Mem.set else_mem2 loc1 ~field:b ~data:val_3 pc in
  
  let common_time = min then_time2 else_time2 - 1 in
  let then_mem1 = Mem.merge then_mem2 else_mem2 common_time cond2 in

  let _ = Mem.set else_mem1 loc1 ~field:b ~data:val_4 pc in

  let common_time = min then_time1 else_time1 - 1 in
  let mem = Mem.merge then_mem1 else_mem1 common_time cond1 in

  Format.printf "Memory: %a@." Mem.pp mem
