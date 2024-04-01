
include Stdlib.Hashtbl

let keys tbl = fold (fun key _ acc -> key :: acc) tbl []
