let factor f =
    let multiply n =
        f * n;
    multiply

let double = factor 2
let triple = factor 3

println double 10
println triple 10
println (factor 4 10)
