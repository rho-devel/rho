expected <- eval(parse(text="structure(c(1, NaN, Inf, 0), .Dim = c(2L, 2L))"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(1, 0), structure(c(1, 0, 0, 1), .Dim = c(2L, 2L)))"));               
do.call(`/`, argv);               
}, o=expected);               

