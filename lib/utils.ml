module Option = struct
  let ( let* ) v f = Option.bind v f
  let ( let+ ) v f = Option.map f v
end

module Generator = struct
  let default_hashtbl_sz = ref 16

  type counter = (unit -> int) * (unit -> unit)

  let make_counter (init : int) (step : int) : counter =
    let counter = ref init in
    let next () =
      let n = !counter in
      counter := n + step;
      n
    and reset () = counter := init in
    (next, reset)

  let make_name_generator (base : string) : unit -> string =
    let (next, _) = make_counter 0 1 in
    fun () -> base ^ string_of_int (next ())
end