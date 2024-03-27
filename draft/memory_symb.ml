(* module M :
     Memory_intf.S
       with type value = Encoding.Expr.t
        and type object_ = Object_symbolic.M.t =
     Memory.Make (Object_symbolic.M)

   include M *)
