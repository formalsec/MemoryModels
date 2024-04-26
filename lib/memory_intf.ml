module type S = sig
  type t
  type value
  type pc_value
  type object_

  val create : unit -> t
  val clone : t -> t
  val merge : t -> t -> pc_value -> t

  (** [alloc m o ] allocates memory for the object [o] in memory [m] and returns
      its location. *)
  val alloc : t -> object_ -> value

  (** [set m l ~field ~data pc] sets the field [field] of the object at location
      [l] in memory [m] to [data]. *)
  val set : t -> value -> field:value -> data:value -> value -> unit

  (** [get m l field pc] gets the value stored at field [field] of the object at
      location [l] in memory [m]. *)
  val get : t -> value -> value -> pc_value -> (value * pc_value) list

  (** [delete m l field pc] deletes field [field] from object at location [l] in
      memory [m]. *)
  val delete : t -> value -> value -> pc_value -> unit

  (** [has_field m l field pc] checks if an object at location [l] in memory [m]
      has a field [field]. *)
  val has_field : t -> value -> value -> pc_value -> value

  val pp : Fmt.t -> t -> unit
end
