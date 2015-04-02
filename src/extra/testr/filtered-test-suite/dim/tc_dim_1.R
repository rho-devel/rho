expected <- eval(parse(text="c(8L, 2L)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 2, 1, 2, 3, 4, 5), .Dim = c(8L, 2L), .Dimnames = list(c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\"), c(\"x1\", \"x2\"))))"));       
do.call(`dim`, argv);       
}, o=expected);       

