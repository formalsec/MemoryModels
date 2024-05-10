open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test memory merge
   Two object:
   - [o] was modified in the then branch
   - [o2] created but never used *)
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
    o.a;          -> Heap.get
   }
                  -> Heap.merge 
   o.d = 6
   ------------obj4---------> R6{{d:6};_}; [ (#y > 3) then R4{{};_;1};R3{{b: 4}; #z: 3; 1} else Empty ]; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
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

  let _ =
    assert (Mem.get else_mem loc a pc = [ (ite (eq a x) val_4 val_3, pc) ])
  in

  let mem = Mem.merge then_mem else_mem 0 cond in

  let _ = Mem.set mem loc ~field:d ~data:val_6 pc in

  (* Format.printf "Memory: %a@." Mem.pp mem; *)
  assert (
    Mem.get mem loc a pc
    = [ (ite (and_ (eq a z) cond) val_3 (ite (eq a x) val_4 val_3), pc) ] );
  assert (
    Mem.get mem loc x pc
    = [ ( ite (eq x d) val_6
            (ite
               (and_ (eq x z) cond)
               val_3
               (ite (and_ (eq x b) cond) val_4 val_4) )
        , pc )
      ] );
  assert (
    Mem.get mem loc b pc
    = [ ( ite
            (and_ (eq b z) cond)
            val_3
            (ite cond val_4 (ite (eq b x) val_4 undef))
        , pc )
      ] );

  assert (
    Mem.get mem loc z pc
    = [ ( ite (eq z d) val_6
            (ite cond val_3 (ite (eq z x) val_4 (ite (eq z a) val_3 undef)))
        , pc )
      ] );
  assert (
    Mem.get mem loc c pc
    = [ (ite (and_ (eq c z) cond) val_3 (ite (eq c x) val_4 undef), pc) ] );
  assert (Mem.get mem loc d pc = [ (val_6, pc) ]);

  (* test get with path condition of y > 3 *)
  assert (
    Mem.get mem loc a cond
    = [ (ite (eq a z) val_3 (ite (eq a x) val_4 val_3), cond) ] );
  assert (
    Mem.get mem loc x cond
    = [ ( ite (eq x d) val_6 (ite (eq x z) val_3 (ite (eq x b) val_4 val_4))
        , cond )
      ] );
  assert (Mem.get mem loc b cond = [ (ite (eq b z) val_3 val_4, cond) ]);

  assert (Mem.get mem loc z cond = [ (ite (eq z d) val_6 val_3, cond) ]);
  assert (
    Mem.get mem loc c cond
    = [ (ite (eq c z) val_3 (ite (eq c x) val_4 undef), cond) ] );
  assert (Mem.get mem loc d cond = [ (val_6, cond) ]);

  (* test get with path condition of y <= 3 *)
  assert (
    Mem.get mem loc a (not_ cond) = [ (ite (eq a x) val_4 val_3, not_ cond) ] );
  assert (
    Mem.get mem loc x (not_ cond) = [ (ite (eq x d) val_6 val_4, not_ cond) ] );
  assert (
    Mem.get mem loc b (not_ cond) = [ (ite (eq b x) val_4 undef, not_ cond) ] );
  assert (
    Mem.get mem loc z (not_ cond)
    = [ ( ite (eq z d) val_6 (ite (eq z x) val_4 (ite (eq z a) val_3 undef))
        , not_ cond )
      ] );
  assert (
    Mem.get mem loc c (not_ cond) = [ (ite (eq c x) val_4 undef, not_ cond) ] );
  assert (Mem.get mem loc d (not_ cond) = [ (val_6, not_ cond) ])
