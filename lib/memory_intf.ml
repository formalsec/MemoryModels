module type S = sig
  type t
  type value
  type loc
  type object_

  val create : unit -> t
  val clone : t -> t
  val insert : t -> object_ -> value
  val remove : t -> loc -> unit
  val set : t -> loc -> object_ -> unit
  val get : t -> loc -> object_ option
  val set_field : t -> loc -> field:value -> data:value -> unit
  val get_field : t -> loc -> value -> (value * value list) list
  val has_field : t -> loc -> value -> value
  val delete_field : t -> loc -> value -> unit
  val pp : Format.formatter -> t -> unit
  val loc : value -> ((value option * loc) list, string) Result.t
  val pp_val : t -> value -> string
end
