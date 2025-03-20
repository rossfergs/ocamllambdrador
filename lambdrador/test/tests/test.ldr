import "stdlib.ld"

let result_list = []
let check_result_list list =
  match list with
    case [] then true
    case true :: bs then check_result_list bs
    case false :: _ then false

println ""

print "reversing a list of [1; 2; 3]: " println (rev [1; 2; 3])
print "functional: "
let result = ([3; 2; 1] = (rev [1;2;3]))
println result
println ""

print "concatenating [1; 2; 3] and [4; 5; 6]: " println (concat [1; 2; 3] [4; 5; 6])
print "functional: "
let result = [1;2;3;4;5;6] = (concat [1; 2; 3] [4; 5; 6])
let result_list = result :: result_list
println result
println ""

print "flattening a tree of `Branch [ `Branch [ `Leaf 1; `Leaf 2]; `Leaf 3]: " println (flatten_tree (`Branch [ `Branch [ `Leaf 1; `Leaf 2]; `Leaf 3]))
print "functional: "
let result = [1;2;3] = (flatten_tree (`Branch [ `Branch [ `Leaf 1; `Leaf 2]; `Leaf 3]))
println result
let result_list = result :: result_list

println ""
let is_less_than_five n = n < 5

print "filtering [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] to have only numbers under 5: "
let filtered=  (filter is_less_than_five [1; 2; 3; 4; 5; 6; 7; 8; 9; 10])
println filtered
print "functional: "
let result = [1;2;3;4] = filtered
println result
let result_list = result :: result_list
println ""

let by_ten n = n * 10
print "mapping [1; 2; 3] to multiply each value by 10: " println (map by_ten [1;2;3])
print "functional: "
let result = [10;20;30] = (map by_ten [1;2;3])
println result
let result_list = result :: result_list
println ""

print "mapping [1; 2; 3] to add one to each value (using anonymous function): " println (map (ld n = n + 1) [1;2;3])
print "functional: "
let result = [2;3;4] = (map (ld n = n + 1) [1;2;3])
println result
let result_list = result :: result_list
println ""

let sub a b = a - b
print "folding left, subtracting a list of [1;2;3] together: " println (foldl sub 0 [1;2;3])
print "functional: "
let result = -6 = (foldl sub 0 [1;2;3])
println result
let result_list = result :: result_list
println ""

print "folding right, subtracting a list of [1; 2; 3] together: " println (foldr sub 0 [1;2;3])
print "functional: "
let result = 2 = (foldr sub 0 [1;2;3])
println result
let result_list = result :: result_list
println ""

print "getting the 3rd index of [0;1;2;3;4;5] (expected = 3): " println (nth [0;1;2;3;4;5] 3) 
print "functional: "
let result = 3 = (nth [0; 1;2;3; 4 ;5] 3) println ""
println result
let result_list = result :: result_list
println ""

print "head of [1;2;3]: " println (head [1;2;3])
print "functional: " 
let result = 1 = (head [1;2;3])
println result
let result_list = result :: result_list
println ""

print "tail of [1;2;3]: " println (tail [1;2;3])
print "functional: " 
let result = [2;3] = (tail [1;2;3])
println result
let result_list = result :: result_list
println ""

let suc n = n + 1
print "function modelling church numeral style encoding, appling successor function to 0 5 times: " println (church 5 suc 0)
print "functional: " 
let result = 5 = (church 5 suc 0)
println result
let result_list = result :: result_list
println ""

print "mem function, to check if a variable is in a list: " println mem [1; 2; 3] 2
print "functional: " 
let result = true = mem [1; 2; 3] 2
println result
let result_list = result :: result_list
println ""

let factor f =
    let x = 1;
    let multiply n =
        f * n;
    multiply

let double = factor 2
let triple = factor 3

println "testing closures: "
print " > double function: " print double 10 print ", "
let result = double 10 = 20
println result
let result_list = result :: result_list

print " > triple function: " print triple 10 print ", "
let result = triple 10 = 30
println result
let result_list = result :: result_list
println ""

print "all functional?: " println check_result_list result_list
