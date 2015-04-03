expected <- eval(parse(text="structure(c(1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(6L, 4L))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0.5, 0.5, 1, 1, 0, 0, 0, 0, 0, 1), .Dim = c(6L, 4L)), c(1, 1, 1, 0, 0, 0))"));                
do.call(`*`, argv);                
}, o=expected);                

