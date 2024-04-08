(* open Encoding

   module Make (O : Object_intf.S with type value = Encoding.Expr.t) = struct
     type value = Encoding.Expr.t
     type pc_value = Encoding.Expr.t
     type object_ = O.t

     type t =
       { parent : t option
       ; map : (int, object_) Hashtbl.t
       ; mutable next : int
       }

     let create () : t = { parent = None; map = Hashtbl.create 512; next = 0 }

     let clone (h : t) : t =
       { parent = Some h; map = Hashtbl.create 16; next = h.next }

     let get_loc (loc : value) : int =
       match Expr.view loc with
       | Val (Int l) -> l
       | _ -> failwith "memory_symbolic.get_loc: Not a location"

     let insert (h : t) (o : object_) : value =
       let next = h.next in
       Hashtbl.replace h.map next o;
       h.next <- h.next + 1;
       Expr.(make @@ Val (Int next))

     let remove (h : t) (l : value) : unit =
       let loc = get_loc l in
       Hashtbl.remove h.map loc

     let set ({ map = h; _ } : t) (l : value) (data : object_) : unit =
       let loc = get_loc l in
       Hashtbl.replace h loc data

     let find (h : t) (l : value) : (object_ * bool) option =
       let open Utils.Option in
       let loc = get_loc l in
       let rec aux { parent; map; _ } loc from_parent =
         match Hashtbl.find_opt map loc with
         | Some o -> Some (o, from_parent)
         | None ->
           let* parent in
           aux parent loc true
       in
       aux h loc false

     let get (h : t) (loc : value) : object_ option =
       let open Utils.Option in
       let+ obj, from_parent = find h loc in
       match from_parent with
       | false -> obj
       | true ->
         let obj = O.clone obj in
         set h loc obj;
         obj

     let has_field (h : t) (loc : value) (field : value) (pc : pc_value) : value =
       Option.fold (get h loc)
         ~some:(fun o -> O.has_field o field pc)
         ~none:Expr.(Bool.v false)

     let set_field (h : t) (loc : value) ~(field : value) ~(data : value) (pc : pc_value) : unit =
       Option.iter
         (fun o ->
           let o' = O.set o ~key:field ~data pc in
           set h loc o' )
         (get h loc)

     let get_field (h : t) (loc : value) (field : value) (pc : pc_value) :
       (value * value list) list =
       let o = get h loc in
       Option.fold o ~none:[] ~some:(fun o -> O.get o field pc)

     let delete_field (h : t) (loc : value) (f : value) (pc : pc_value) : unit =
       let obj = get h loc in
       Option.iter
         (fun o ->
           let o' = O.delete o f pc in
           set h loc o' )
         obj

     let pp_loc (fmt : Fmt.t) (loc : value) : unit = Expr.pp fmt loc

     let rec pp (fmt : Fmt.t) ({ map; parent; _ } : t) =
       let open Fmt in
       let pp_v fmt (key, data) = fprintf fmt "%a: %a" pp_int key O.pp data in
       let pp_parent fmt v =
         pp_opt (fun fmt h -> fprintf fmt "%a@\n<-@\n" pp h) fmt v
       in
       fprintf fmt "%a{ %a }" pp_parent parent
         (pp_hashtbl ~pp_sep:pp_comma pp_v)
         map

     let pp_val (fmt : Fmt.t) (h : t) (l : value) : unit =
       let open Fmt in
       match get h l with
       | None -> fprintf fmt "%a" pp_loc l
       | Some o -> fprintf fmt "%a -> %a" pp_loc l O.pp o
   end

   module M :
     Memory_intf.S
       with type value = Encoding.Expr.t
        and type object_ = Object_mwl.M.t =
     Make (Object_symbolic.M)

   include M *)
