```js
Caso I

o = {}; 
o.a = 3; 
if (#y > 0) {
  o.a = 1; 
} else {
  o.a = 2; 
}

o.a = 
true, None, [ (true, 1) ]
true, None, [ (true, 2) ]
```

```js
Caso II

o = {}; 
o.a = 3; 
if (#y > 0) {
  o.b = 1;
} else {
  o.c = 2; 
}

o.a
false, None, [ ]
false  None, [ ]
```



```js
Caso III

o = {}; 
o.a = 3; 
if (#y > 0) {
  o.a = 1;
} else {
  o.c= 2; 
}

o.a
true , None, [ ]
false, None, [ ]
```

```js
Caso IV

o = {}; 
o.a = 3; 
if (#y > 0) {
  o.b = 1;
} else {
  o.a= 2; 
}

o.a
false, None, [ ]
true , None, [ ]
```

```js
Caso V

o = {}; 
o.a = 3; 
if (#y > 0) {
  o.a = 1; 
} else {
  o[#z] = 2; 
}

o.a
true , None, [(true, 1)]
false, Some, [(#z = a, 2)]
```

```js
Caso VI

o = {}; 
o.a = 3; 
if (#y > 0) {
  o[#x] = 1;
} else {
  o.a = 2; 
}

o.a
false, Some, [ (#x = a, 1) ]
true , None, [ (true, 2) ]
```

```js
Caso VII

o = {}; 
o.a = 3; 
if (#y > 0) {
  o.b = 1;
} else {
  o[#z]= 2; 
}

o.a
false, None, [ ]
false, Some, [(#z = a, 2)]
```

```js
Caso VIII

o = {}; 
o.a = 3; 
if (#y > 0) {
  o[#x] = 1;
} else {
  o.c= 2; 
}

o.a
false, Some, [ (#x = a, 1) ]
false, None, [ ]
```

```js
Caso IX

o = {}; 
o.a = 3; 
if (#y > 0) {
  o[#x] = 1; 
} else {
  o[#z] = 2; 
}

o.a
false, Some, [(#x = a, 1)]
false, Some, [(#z = a, 2)]
```

```ocaml
let rec get_object 
  (p : value) 
  (pc : pc_value)
  (o : t) :
    bool * pc_value * (pc_value * value option) list =

  List.fold_left
    (fun (r, s_pc, pvs) orec ->
      if (not r) 
       then ( 
      	let r', s_pc', pvs' = get_record orec s_pc p in  
        (r', and_ s_pc  s_pc', pvs @ pvs') 
       ) else (r, explored_pc, pvs)
    ) (false, pc, []) o 
and 

get_record (orec : record) (pc : pc_value) (p : value) :
    bool * pc_value * (pc_value * value option) list =
      
  match r with
  | Rec { concrete; symbolic; _ } -> (
    let get_concrete = get_concrete concrete in
    match symbolic with
    | Some (p', v) ->
        if pc => eq p p' then (true, _true, [ (_true, v) ])
        else if is_sat [ pc; eq p p' ] then
          let b, s_pc, pvs = get_concrete (and_ pc (ne p p')) p in
          let s_pc' = _nary_and [ s_pc; (ne p p') ]  in
          (b, s_pc', (eq p p', v) :: pvs)
        else get_concrete pc p
      | None -> get_concrete pc p )
  
  | If { cond; then_; else_; _ } ->
    let add_cond cond = List.map (fun (pc, v) -> (and_ pc cond, v)) in
    
    if pc => cond 
      then 
    	get_object p pc then_ 
      else if pc => _not cond 
        then get_object p pc else_ 
        else (
          let then_b, then_pc, then_pvs = get_if then_ (_and pc cond) in
          let else_b, else_pc, else_pvs = get_if else_ (_and pc (not_ cond)) in
          let then_pvs, else_pvs = (add_cond cond then_pvs, add_cond (not_ cond) else_pvs) in 
		  let pvs' = then_pvs @ else_pvs in
 
 		  let b', s_pc' = match then_b, else_b with
 		    | true, true -> true, boolean true
 		    | true, false -> false, and_ (not_cond) else_pc
 		    | false, true -> false, and_ cond then_pc 
 		    | false, false -> false, or_ (and_ cond then_pc) (and_ (not_ cond) else_pc) in 
          
          b', s_pc', pvs')    
    


 let get_concrete 
   (concrete : concrete_table) 
   (pc : pc_value) 
   (p : value) :
       bool * pc_value * (pc_value * value option) list =
    
   let find_opt = Hashtbl.find_opt concrete in
   let find = Hashtbl.find concrete in
   let bool_true = boolean true in

    match Expr.view p with
    | Val (Str s) -> (
      let v = find_opt s in
      match v with
      | Some v' -> (true, bool_true, [ (bool_true, v') ])
      | _ -> (false, bool_true, []) )
    | _ -> (
      let keys = Hashtbl.keys concrete in
      let keys' = List.filter (fun k -> is_sat [ eq p (str k); pc ]) keys in
      match keys' with
      | [ k ] ->
        if pc => eq p (str k) then (true, bool_true, [ (bool_true, find k) ])
        else (false, (ne p (str k)), [ (eq p (str k), find k) ])
      | _ ->
        (false, all_different p keys', List.map (fun k -> (eq p (str k), find k)) keys' ) 
    )
```