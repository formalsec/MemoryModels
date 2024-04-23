open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M

(* Test case 6-0: pc = true
   Simple test of branching and merging
   assertion with pc = true and other path conditions*)
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
   o = {}
   o.a = 3;
   o[#x] = 4
   ------------obj1---------> R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
   if (#y > 3) {
     o.b = 4;
     o[#z] = 3;
   ------------obj2---------> R4{{};_;1};R3{{b: 4}; #z: 3; 1}; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
   } else {
     o.c = 5;
   ------------obj3---------> R5{{c:5};_;2}; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}
   }
   o.d = 6
   ------------obj4---------> R6{{d:6};_} -> [ (R4, #y > 3); (R5, #y <= 3) ]

   R6{{d:6};_}; [ (#y > 3) then R4{{};_;1};R3{{b: 4}; #z: 3; 1} else R5{{c:5};_;2} ]; R2{{};_; 0}; R1:{{a:3}; #x:4; 0}

   o.b with #y > 3 --> ITE(#b == #z , 3, 4)

   o.a --> [#y > 3 && #z = a, 3]; [#x = a, 4]; [True, 3]  -> ITE(#y > 3 && #z = a, 3, ITE(#x = a, 4, 3))
  *)
  let obj = Obj.create () in
  let obj, pc = get_obj (Obj.set obj ~field:a ~data:val_3 pc) in
  let obj, pc = get_obj (Obj.set obj ~field:x ~data:val_4 pc) in

  let then_obj = Obj.clone obj in
  let else_obj = Obj.clone obj in

  let then_obj, pc = get_obj (Obj.set then_obj ~field:b ~data:val_4 pc) in
  let then_obj, pc = get_obj (Obj.set then_obj ~field:z ~data:val_3 pc) in

  let else_obj, pc = get_obj (Obj.set else_obj ~field:c ~data:val_5 pc) in

  let merged_obj = Obj.merge then_obj else_obj cond in

  let merged_obj, pc = get_obj (Obj.set merged_obj ~field:d ~data:val_6 pc) in

  (* test has_field *)
  assert (
    Obj.has_field merged_obj a pc
    = ite
        (and_ (eq a z) cond)
        (value_bool true)
        (ite (eq a x) (value_bool true) (value_bool true)) );
  assert (
    Obj.has_field merged_obj x pc
    = ite (eq x d) (value_bool true)
        (ite
           (and_ (eq x z) cond)
           (value_bool true)
           (ite
              (and_ (eq x b) cond)
              (value_bool true)
              (ite
                 (and_ (eq x c) (not_ cond))
                 (value_bool true) (value_bool true) ) ) ) );
  assert (
    Obj.has_field merged_obj b pc
    = ite
        (and_ (eq b z) cond)
        (value_bool true)
        (ite cond (value_bool true)
           (ite (eq b x) (value_bool true) (value_bool false)) ) );

  assert (
    Obj.has_field merged_obj z pc
    = ite (eq z d) (value_bool true)
        (ite cond (value_bool true)
           (ite
              (and_ (eq z c) (not_ cond))
              (value_bool true)
              (ite (eq z x) (value_bool true)
                 (ite (eq z a) (value_bool true) (value_bool false)) ) ) ) );
  assert (
    Obj.has_field merged_obj c pc
    = ite
        (and_ (eq c z) cond)
        (value_bool true)
        (ite (not_ cond) (value_bool true)
           (ite (eq c x) (value_bool true) (value_bool false)) ) );
  assert (Obj.has_field merged_obj d pc = value_bool true);

  (* test get *)
  assert (
    Obj.get merged_obj a pc
    = [ (ite (and_ (eq a z) cond) val_3 (ite (eq a x) val_4 val_3), pc) ] );
  assert (
    Obj.get merged_obj x pc
    = [ ( ite (eq x d) val_6
            (ite
               (and_ (eq x z) cond)
               val_3
               (ite
                  (and_ (eq x b) cond)
                  val_4
                  (ite (and_ (eq x c) (not_ cond)) val_5 val_4) ) )
        , pc )
      ] );
  assert (
    Obj.get merged_obj b pc
    = [ ( ite
            (and_ (eq b z) cond)
            val_3
            (ite cond val_4 (ite (eq b x) val_4 undef))
        , pc )
      ] );

  assert (
    Obj.get merged_obj z pc
    = [ ( ite (eq z d) val_6
            (ite cond val_3
               (ite
                  (and_ (eq z c) (not_ cond))
                  val_5
                  (ite (eq z x) val_4 (ite (eq z a) val_3 undef)) ) )
        , pc )
      ] );
  assert (
    Obj.get merged_obj c pc
    = [ ( ite
            (and_ (eq c z) cond)
            val_3
            (ite (not_ cond) val_5 (ite (eq c x) val_4 undef))
        , pc )
      ] );
  assert (Obj.get merged_obj d pc = [ (val_6, pc) ]);

  (* test get with path condition of y > 3 *)
  assert (
    Obj.get merged_obj a cond
    = [ (ite (eq a z) val_3 (ite (eq a x) val_4 val_3), cond) ] );
  assert (
    Obj.get merged_obj x cond
    = [ ( ite (eq x d) val_6 (ite (eq x z) val_3 (ite (eq x b) val_4 val_4))
        , cond )
      ] );
  assert (Obj.get merged_obj b cond = [ (ite (eq b z) val_3 val_4, cond) ]);

  assert (Obj.get merged_obj z cond = [ (ite (eq z d) val_6 val_3, cond) ]);
  assert (
    Obj.get merged_obj c cond
    = [ (ite (eq c z) val_3 (ite (eq c x) val_4 undef), cond) ] );
  assert (Obj.get merged_obj d cond = [ (val_6, cond) ]);

  (* test get with path condition of y <= 3 *)
  assert (
    Obj.get merged_obj a (not_ cond) = [ (ite (eq a x) val_4 val_3, not_ cond) ] );
  assert (
    Obj.get merged_obj x (not_ cond)
    = [ (ite (eq x d) val_6 (ite (eq x c) val_5 val_4), not_ cond) ] );
  assert (
    Obj.get merged_obj b (not_ cond) = [ (ite (eq b x) val_4 undef, not_ cond) ] );
  assert (
    Obj.get merged_obj z (not_ cond)
    = [ ( ite (eq z d) val_6
            (ite (eq z c) val_5
               (ite (eq z x) val_4 (ite (eq z a) val_3 undef)) )
        , not_ cond )
      ] );
  assert (Obj.get merged_obj c (not_ cond) = [ (val_5, not_ cond) ]);
  assert (Obj.get merged_obj d (not_ cond) = [ (val_6, not_ cond) ])
