open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

(* Test memory merge
   Two object:
   - [o] was modified in the else branch
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
    o.b;      -> Heap.get 
   } else {
    o[#z] = 5;    -> Heap.set
   }
                  -> Heap.merge 
   o.d = 6
   ------------obj4---------> R6{{d:6};_}; [ (#y > 3) then R3{Empty(1)} else R5{Empty(2)}; R4{c:5};_;2} ]; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
  *)
  let mem = Mem.create () in
  let loc = Mem.alloc mem in
  let _ = Mem.set mem loc ~field:a ~data:val_3 pc in
  let _ = Mem.set mem loc ~field:x ~data:val_4 pc in
  let _loc2 = Mem.alloc mem in

  let then_mem = Mem.clone mem 1 in
  let else_mem = Mem.clone mem 1 in

  let _ =
    assert (Mem.get then_mem loc b pc = [ (ite (eq b x) val_4 undef, pc) ])
  in

  let _ = Mem.set else_mem loc ~field:z ~data:val_5 pc in

  let mem = Mem.merge then_mem else_mem 1 cond in

  let _ = Mem.set mem loc ~field:d ~data:val_6 pc in

  (* Format.printf "Memory: %a@." Mem.pp mem; *)
  assert (
    Mem.get mem loc a pc
    = [ (ite (and_ (eq a z) (not_ cond)) val_5 (ite (eq a x) val_4 val_3), pc) ] );
  assert (
    Mem.get mem loc x pc
    = [ (ite (eq x d) val_6 (ite (and_ (eq x z) (not_ cond)) val_5 val_4), pc) ] );
  assert (
    Mem.get mem loc b pc
    = [ (ite (and_ (eq b z) (not_ cond)) val_5 (ite (eq b x) val_4 undef), pc) ] );

  assert (
    Mem.get mem loc z pc
    = [ ( ite (eq z d) val_6
            (ite (not_ cond) val_5
               (ite (eq z x) val_4 (ite (eq z a) val_3 undef)) )
        , pc )
      ] );
  assert (
    Mem.get mem loc c pc
    = [ (ite (and_ (eq c z) (not_ cond)) val_5 (ite (eq c x) val_4 undef), pc) ] );
  assert (Mem.get mem loc d pc = [ (val_6, pc) ]);

  (* test get with path condition of y > 3 *)
  assert (Mem.get mem loc a cond = [ (ite (eq a x) val_4 val_3, cond) ]);
  assert (Mem.get mem loc x cond = [ (ite (eq x d) val_6 val_4, cond) ]);
  assert (Mem.get mem loc b cond = [ (ite (eq b x) val_4 undef, cond) ]);

  assert (
    Mem.get mem loc z cond
    = [ ( ite (eq z d) val_6 (ite (eq z x) val_4 (ite (eq z a) val_3 undef))
        , cond )
      ] );
  assert (Mem.get mem loc c cond = [ (ite (eq c x) val_4 undef, cond) ]);
  assert (Mem.get mem loc d cond = [ (val_6, cond) ]);

  (* test get with path condition of y <= 3 *)
  assert (
    Mem.get mem loc a (not_ cond)
    = [ (ite (eq a z) val_5 (ite (eq a x) val_4 val_3), not_ cond) ] );
  assert (
    Mem.get mem loc x (not_ cond)
    = [ (ite (eq x d) val_6 (ite (eq x z) val_5 val_4), not_ cond) ] );
  assert (
    Mem.get mem loc b (not_ cond)
    = [ (ite (eq b z) val_5 (ite (eq b x) val_4 undef), not_ cond) ] );
  assert (
    Mem.get mem loc z (not_ cond) = [ (ite (eq z d) val_6 val_5, not_ cond) ] );
  assert (
    Mem.get mem loc c (not_ cond)
    = [ (ite (eq c z) val_5 (ite (eq c x) val_4 undef), not_ cond) ] );
  assert (Mem.get mem loc d (not_ cond) = [ (val_6, not_ cond) ])
