expected <- eval(parse(text="structure(c(NaN, NaN, NaN, NaN), .Dim = c(4L, 1L), .Dimnames = list(NULL, \"L75\"))"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(c(0.5, 0.5, 0.5, 0.5), gradient = structure(c(NaN, NaN, NaN, NaN), .Dim = c(4L, 1L), .Dimnames = list(NULL, \"L75\"))), \"gradient\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

