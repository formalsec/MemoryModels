module type S = sig
  type t
  type value
  type pc_value

  val create : unit -> t
  val clone : t -> t

  (** [set o ~field ~data pc] sets the field [field] of the object [o] to [data]
      and returns a list of a pair of resulted object and respective path
      condition *)
  val set : t -> field:value -> data:value -> pc_value -> (t * pc_value) list

  (** [get o field pc] gets the value of the field [field] of the object [o].
      (it can be an ite expression) *)
  val get : t -> value -> pc_value -> (value * pc_value) list

  (** [delete o field pc] deletes the field [field] in the object [o] and
      returns a list of a pair of resulted object and respective path condition *)
  val delete : t -> value -> pc_value -> (t * pc_value) list

  val to_list : t -> (value * value) list

  (** [get_fields o] returns all fields stored in object [o]. *)
  val get_fields : t -> value list

  (** [has_field o field pc] checks it the object [o] contains the field
      [field]. *)
  val has_field : t -> value -> value -> value

  val pp : Fmt.t -> t -> unit
  val to_string : t -> string
  val to_json : t -> string
end
