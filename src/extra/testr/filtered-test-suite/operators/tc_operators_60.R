expected <- eval(parse(text="structure(c(0L, -1L, -2L, 1L, 0L, -1L, 2L, 1L, 0L), .Dim = c(3L, 3L))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L), .Dim = c(3L, 3L)), structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L), .Dim = c(3L, 3L)))"));               
do.call(`-`, argv);               
}, o=expected);               

