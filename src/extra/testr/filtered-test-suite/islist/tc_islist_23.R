expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), .Dim = c(10L, 2L), .Dimnames = list(NULL, c(\"x\", \"y\"))))"));       
do.call(`is.list`, argv);       
}, o=expected);       

