expected <- eval(parse(text="structure(c(0, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, -1, 0, 1, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 1), .Dim = c(7L, 7L), .Dimnames = list(NULL, NULL))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1), .Dim = c(7L, 7L), .Dimnames = list(NULL, NULL)), c(1, 0, 0, 0, 0, 0, 0))"));               
do.call(`-`, argv);               
}, o=expected);               

