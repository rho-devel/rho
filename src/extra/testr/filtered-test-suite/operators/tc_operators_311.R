expected <- eval(parse(text="structure(c(1, -2, -2, 4), .Dim = c(2L, 2L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(-1, 2), .Dim = c(2L, 1L), .Dimnames = list(NULL, \"x\")), c(-1, 2))"));              
do.call(`%*%`, argv);              
}, o=expected);              

