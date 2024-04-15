open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M

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

  print_obj Obj.pp merged_obj;
  (* Format.printf "has_field: %a\n" Encoding.Expr.pp (Obj.has_field merged_obj x pc); *)
  (* print_get a (Obj.get merged_obj x pc) *)
  print_get a (Obj.get merged_obj x pc)
