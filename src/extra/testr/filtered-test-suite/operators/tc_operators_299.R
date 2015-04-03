expected <- eval(parse(text="structure(c(1, 1, 1, 1, 46656, 1, 1, 1, 1, 46656, 1, 1, 1, 1, 46656, 1), .Dim = c(4L, 4L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(0, 0, 0, 0, 6, 0, 0, 0, 0, 6, 0, 0, 0, 0, 6, 0), .Dim = c(4L, 4L)), structure(c(0, 0, 0, 0, 6, 0, 0, 0, 0, 6, 0, 0, 0, 0, 6, 0), .Dim = c(4L, 4L)))"));              
do.call(`^`, argv);              
}, o=expected);              

