expected <- eval(parse(text="structure(c(NA, 2, NA, 1, NA, 0), .Dim = 2:3)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(NA, 2, NA, 1, NA, 0), .Dim = 2:3))"));               
do.call(`(`, argv);               
}, o=expected);               

