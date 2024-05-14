module type S = sig
  type t
  type value
  type pc_value
  type object_

  val create : unit -> t
  val clone : t -> int -> t
  val merge : t -> t -> int -> pc_value -> t

  (** [alloc m] allocates memory for a new object [o] in memory [m] and returns
      its location. *)
  val alloc : t -> value

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

  (** [get_fields m l] returns all fields of the object at location [l] in memory [m] *)
  val get_fields : t -> value  -> value list

  (** [to_list m l] returns a list of pairs of fields and the respective value of the object at location [l]. *)
  val to_list : t -> value -> (value * value) list

  val pp : Fmt.t -> t -> unit

  val pp_val : t -> Fmt.t -> value -> unit
end
