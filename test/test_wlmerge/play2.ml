open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

let () =
  let x = key_s_int "x" in
  let s1 = key_s "s1" in
  let _s2 = key_s "s2" in
  let p = key_c "p" in
  let val_3 = value_int 3 in
  let val_x_1 = plus x (value_int 1) in
  let val_4 = value_int 4 in
  let pc = value_bool true in
  let _cond = gt x (value_int 0) in
  (*
     o := {};    // o : {{}, -, 0}
  o.p := 3;   // o : {{p:3}, -, 0}
  o2 := {};    // o : {{p:3}, -, 0} || o2 : {{}, -, 0}
  if (#x > 0){ 
  ---------time: 1------> {content: [None, None], time: 1}; {content: [{{p:3}, -, 0} || {{}, -, 0}], time: 0}
    o[#s1] := #x +1      // o : {{}, -, 1}; {{}, #s1: #x+1, 1}; {{p:3}, -, 0} || o2 : {{}, -, 0}  
  ----------------------> {content: [{{}, -, 1}; {{}, #s1: #x+1, 1}; {{p:3}, -, 0}, None], time: 1}; {content: [{{p:3}, -, 0} || {{}, -, 0}], time: 0}
  } else {
  ---------time: 2------> {content: [None, None], time: 2}; {content: [{{p:3}, -, 0} || {{}, -, 0}], time: 0}
    o.p := o.p + 1;        // o : {{p: 4}, -, 2}; {{p:3}, -, 0} || o2 : {{}, -, 0}  
  ----------------------> {content: [{{p: 4}, -, 2}; {{p:3}, -, 0}, None], time: 2}; {content: [{{p:3}, -, 0} || {{}, -, 0}], time: 0}
  }
  ------------------->    o : {{}, -, 4}; ITE[3](#x > 0 then {{}, -, 1}; {{}, #s1: #x+1, 1} else {{p: 4}, -, 2}); {{p:3}, -, 0} 
                      || o2 : {{}, -, 0}  
  o[#s2] := o.p + 3; 
  ------------------->    o : {{}, _, 4}; {{}, #s2: ???_1, 4}; ITE[3](#x > 0 then {{}, -, 1}; {{}, #s1: #x+1, 1} else {{p: 4}, -, 2}); {{p:3}, -, 0} 
                      || o2 : {{}, -, 0}  
  o.r := 4;
  ------------------->    o : {{r: 4}, _, 4}; {{}, #s2: ???, 4}; ITE[3](#x > 0 then {{}, -, 1}; {{}, #s1: #x+1, 1} else {{p: 4}, -, 2}); {{p:3}, -, 0} 
                      || o2 : {{}, -, 0}  
  o2.a := o[#s3]
  ------------------->    o : {{r: 4}, _, 4}; {{}, #s2: ???, 4}; ITE[3](#x > 0 then {{}, -, 1}; {{}, #s1: #x+1, 1} else {{p: 4}, -, 2}); {{p:3}, -, 0} 
                      || o2 : {{a: ???_2}, _, 4}; {{}, -, 0}  

  TODO: ???_1 o que guarda aqui, já depende da condição
  *)
  let mem = Mem.create () in
  let o = Obj.create () in
  let o2 = Obj.create () in
  let loc = Mem.alloc mem o in
  let _ = Mem.set mem loc ~field:p ~data:val_3 pc in
  let _loc2 = Mem.alloc mem o2 in
  let then_mem = Mem.clone mem in
  let else_mem = Mem.clone mem in
  let _then_mem = Mem.set then_mem loc ~field:s1 ~data:val_x_1 pc in
  let _else_mem = Mem.set else_mem loc ~field:p ~data:val_4 pc in
  ()
