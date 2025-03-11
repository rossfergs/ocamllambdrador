let not b = if b then false else true

let rev l =
  let aux acc list =
    match list with
      case [] then acc
      case x :: xs then
        aux (x :: acc) xs;
  aux [] l

let concat l1 l2 =
  let aux acc l =
    match l with
      case [] then rev acc
      case x :: xs then
        aux (x :: acc) xs;
  aux (rev l1) l2

let flatten_tree tree =
  match tree with
    case `Branch [l; r] then
      let ll = flatten_tree l;
      let rl = flatten_tree r;
      concat ll rl
    case `Leaf n then [n]

let filter func list =
  let aux acc l =
    match l with
      case [] then rev acc
      case x :: xs when func x = true then
        aux (x :: acc) xs
      case _ :: xs then
        aux acc xs;
  aux [] list

let map func list =
  let aux acc l =
    match l with
      case [] then rev acc
      case x :: xs then aux ((func x) :: acc) xs;
  aux [] list

let foldl func init list =
  let aux acc l =
    match l with
      case [] then acc
      case x :: xs then aux (func acc x) xs;
  aux init list

let foldr func init list =
  let aux acc l =
    match l with
      case [] then acc
      case x :: xs then aux (func x acc) xs;
  aux init list

let nth list n =
  let aux counter l =
    match l with
      case x :: [] then x
      case x :: _ when counter <= 0 then x
      case x :: xs then aux (counter-1) xs
      case [] then print "empty list"; [];
  aux n list

let head list =
  match list with
    case x :: _ then
      x
    case [] then
      []

let tail list =
  match list with
    case _ :: xs then
      xs
    case [] then
      []

let church n f v = 
  match n with
    case _ when n > 0 then
      church (n-1) f (f v)
    case _ then v



@
let flatten list = 
  let aux acc l = 
    match l with
      case (x :: xs) :: r then
        print (x :: xs);
        [];
  aux [] list
@

println ""

print "reversing a list of [1; 2; 3]: " println (rev [1; 2; 3])
println ""

print "concatenating [1; 2; 3] and [4; 5; 6]: " println (concat [1; 2; 3] [4; 5; 6])
println ""

print "flattening a tree of `Branch [ `Branch [ `Leaf 1; `Leaf 2]; `Leaf 3]: " println (flatten_tree (`Branch [ `Branch [ `Leaf 1; `Leaf 2]; `Leaf 3]))

println ""
let is_less_than_five n = n < 5

print "filtering [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] to have only numbers under 5: "
println (filter is_less_than_five [1; 2; 3; 4; 5; 6; 7; 8; 9; 10])
println ""

let by_ten n = n * 10
print "mapping [1; 2; 3] to multiply each value by 10: " println (map by_ten [1;2;3])
println ""
print "mapping [1; 2; 3] to add one to each value (using anonymous function): " println (map (ld n = n + 1) [1;2;3])
println ""

let sub a b = a - b
print "folding left, subtracting a list of [1;2;3] together: " println (foldl sub 0 [1;2;3])
println ""

print "folding right, subtracting a list of [1; 2; 3] together: " println (foldr sub 0 [1;2;3]) println ""

print "getting the 3rd index of [0;1;2;3;4;5] (expected = 3): " println (nth [0;1;2;3;4;5] 3) println ""

print "head of [1;2;3]: " println (head [1;2;3]) println ""
print "tail of [1;2;3]: " println (tail [1;2;3]) println ""

let suc n = n + 1
print "function modelling church numeral style encoding, appling successor function to 0 5 times: " println (church 5 suc 0)
