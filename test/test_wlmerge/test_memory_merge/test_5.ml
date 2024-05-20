open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test memory merge
   Two object:
   - [o] was modified in the then branch
   - [o2] was modified in the both branch *)
let () =
  let x = key_s "x" in
  let y = key_s_int "y" in
  let z = key_s "z" in
  let w = key_s "w" in
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
  o = {}          -> Heap.alloc  
  o.a = 3;        -> Heap.set
  o[#x] = 4       -> Heap.set 
  o2 = {}         -> Heap.alloc  
   if (#y > 3) {
    o2.b = 6;     -> Heap.set
    o.b = 7;      -> Heap.set
   } else {
    o[#a];        -> Heap.get
    o2[#z] = 7;   -> Heap.set
   }
                  -> Heap.merge 
   o.d = 6

   -----> o  = {{d:6};_;0}; {If #y > 3 then {{b:7};_;1} else []]; {Empty}{{a:3}; #x:4 ; 0}}
   -----> o2 = {If #y > 3 then {{b:6};_;1} else Empty;{{};#z:7;2}; Empty(0)
   o[#w]
   o2[#w]
  *)
  let mem = Mem.create () in
  let loc1 = Mem.alloc mem in
  let _ = Mem.set mem loc1 ~field:a ~data:val_3 pc in
  let _ = Mem.set mem loc1 ~field:x ~data:val_4 pc in
  let loc2 = Mem.alloc mem in

  let then_mem = Mem.clone mem 1 in
  let else_mem = Mem.clone mem 1 in

  let _ = Mem.set then_mem loc2 ~field:b ~data:val_6 pc in
  let _ = Mem.set then_mem loc1 ~field:b ~data:val_7 pc in

  let _ =
    assert (Mem.get else_mem loc1 a pc = [ (ite (eq a x) val_4 val_3, pc) ])
  in
  let _ = Mem.set else_mem loc2 ~field:z ~data:val_7 pc in

  let mem = Mem.merge then_mem else_mem 1 cond in

  let _ = Mem.set mem loc1 ~field:d ~data:val_6 pc in

  (* Format.printf "Memory: %a@." Mem.pp mem; *)
  assert (
    Mem.get mem loc1 w pc
    = [ ( ite (eq w d) val_6
            (ite
               (and_ (eq w b) cond)
               val_7
               (ite (eq w x) val_4 (ite (eq w a) val_3 undef)) )
        , pc )
      ] );
  assert (
    Mem.get mem loc2 w pc
    = [ ( ite
            (and_ (eq w b) cond)
            val_6
            (ite (and_ (eq w z) (not_ cond)) val_7 undef)
        , pc )
      ] )
