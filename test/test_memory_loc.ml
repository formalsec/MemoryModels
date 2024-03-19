open Utils
open Memory_models
module Mem = Memory_symbolic

(* Test loc function *)
let () =
  let x = key_s "x" in
  let y = key_s "y" in
  let a = key_s "a" in
  let b = key_s "b" in
  let c = key_s "c" in

  assert (Mem.loc (value_int 10) = Ok [ (None, 10) ]);

  assert (
    Mem.loc (ite (eq x a) (value_int 10) (value_int 20))
    = Ok [ (Some (eq x a), 10); (Some (ne (eq x a)), 20) ] );
  assert (
    Mem.loc
      (ite (eq x a) (value_int 10) (ite (eq x b) (value_int 20) (value_int 30)))
    = Ok
        [ (Some (eq x a), 10)
        ; (Some (and_ (ne (eq x a)) (eq x b)), 20)
        ; (Some (and_ (ne (eq x a)) (ne (eq x b))), 30)
        ] );

  assert (Result.is_error (Mem.loc (value_bool true)));
  assert (Result.is_error (Mem.loc (value_bool false)));
  assert (Result.is_error (Mem.loc (value_str "10")));
  assert (Result.is_error (Mem.loc (value_str "hello")));
  assert (Result.is_error (Mem.loc (eq (value_int 200) (value_int 200))));
  assert (
    Result.is_error (Mem.loc (ite (eq x a) (value_str "10") (value_int 20))) );
  assert (
    Result.is_error (Mem.loc (ite (eq y y) (value_bool false) (value_int 20))) );
  assert (
    Result.is_error
      (Mem.loc
         (ite (eq y b)
            (ite (eq x b) (value_int 20) (value_int 30))
            (value_int 20) ) ) );
  assert (
    Result.is_error
      (Mem.loc
         (ite (eq y b)
            (ite (eq y a) (value_int 20) (value_int 30))
            (ite (eq y c) (value_int 40) (value_int 50)) ) ) )
