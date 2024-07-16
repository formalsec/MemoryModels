open Test.Utils
open Memory_models
module Obj = Object_wlmerge.M
module Mem = Memory_wlmerge

let () =
  let y = key_s_int "y" in
  let a = key_c "a" in
  let _d = key_c "d" in
  let val_3 = value_int 3 in
  let _val_6 = value_int 6 in
  let pc = value_bool true in
  let cond = gt y (value_int 3) in
  
  let loc = ite cond (value_int 1) (ite (not_ cond) (value_int 2) (undef)) in
  let mem = Mem.create () in
  let mem = Mem.set mem loc ~field:a ~data:val_3 pc in
  mem