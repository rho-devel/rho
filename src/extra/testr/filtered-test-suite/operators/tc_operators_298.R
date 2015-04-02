expected <- eval(parse(text="structure(c(1, 0.8, 0.64, 0.8, 1, 0.8, 0.64, 0.8, 1), .Dim = c(3L, 3L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(0.8, structure(c(0L, 1L, 2L, 1L, 0L, 1L, 2L, 1L, 0L), .Dim = c(3L, 3L)))"));              
do.call(`^`, argv);              
}, o=expected);              

