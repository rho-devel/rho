expected <- eval(parse(text="c(1, 2, 3, 4, 5, 0, 7, 8, 9, 10, 11, 12)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(1, 2, 3, 4, 5, 0, 7, 8, 9, 10, 11, 12), .Dim = c(4L, 3L)))"));               
do.call(`as.double`, argv);               
}, o=expected);               

