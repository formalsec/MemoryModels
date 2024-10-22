module type S = sig
  type t
  type value
  type pc_value
  type object_

  val create : unit -> t
  val clone : t -> t

  (** [insert m o ] inserts the object [o] into memory [m] and returns its
      location. *)
  val insert : t -> object_ -> value

  (** [remove m l] removes the object at location [l] from memory [m]. *)
  val remove : t -> value -> unit

  (** [set m l o] sets the object at location [l] in memory [m] to objcet [o]. *)
  val set : t -> value -> object_ -> unit

  (** [get m l o] get the object at location [l] in memory [m]. *)
  val get : t -> value -> object_ option

  (** [set_field m l ~field ~data pc] sets the field [field] of the object at
      location [l] in memory [m] to [data]. *)
  val set_field : t -> value -> field:value -> data:value -> value -> unit

  (** [get_field m l field pc] gets the value stored at field [field] of the
      object at location [l] in memory [m]. *)
  val get_field : t -> value -> value -> pc_value -> (value * value list) list

  (** [has_field m l field pc] checks if an object at location [l] in memory [m]
      has a field [field]. *)
  val has_field : t -> value -> value -> pc_value -> value

  (** [delete_field m l field pc] deletes field [field] from object at location
      [l] in memory [m]. *)
  val delete_field : t -> value -> value -> pc_value -> unit

  val pp : Fmt.t -> t -> unit
  val pp_val : Fmt.t -> t -> value -> unit
end
