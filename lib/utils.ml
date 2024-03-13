module Option = struct
  let ( let* ) v f = Option.bind v f
  let ( let+ ) v f = Option.map f v
end
