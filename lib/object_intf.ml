module type S = sig
  type t
  type value

  val create : unit -> t
  val clone : t -> t
  val is_empty : t -> bool
  val to_list : t -> (value * value) list
  val get_fields : t -> value list
  val has_field : t -> value -> value
  val set : t -> key:value -> data:value -> t
  val get : t -> value -> (value * value list) list
  val delete : t -> value -> t
  val pp : Fmt.t -> t -> unit
  val to_string : t -> string
  val to_json : t -> string
end