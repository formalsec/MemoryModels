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