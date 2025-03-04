let rec n inc = print n; if n < 10 then rec (n + inc) inc else 0
println "a"
print (rec 0 1)
println "b"

