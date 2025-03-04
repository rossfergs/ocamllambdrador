let apply f a b = f a b

let add a b = a + b

let sub a b = a - b

println "using functions as values on input"
println ""
println "function 'apply' takes in 3 parameters, and applies the last 2 inputs to the first input"
println ""
println "by enclosing a parameter in parenthesis parameters can be given, the function "
println "will be evaluated and the resulting value will be passed"
println ""

println "apply add 1 (sub 2 2) = "
println (apply add 1 (sub 2 2))
