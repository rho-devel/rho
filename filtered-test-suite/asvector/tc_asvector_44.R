expected <- eval(parse(text="c(0.1, 0.8, 1, 0.5, 0.8, 1, 0, 0.5, 1)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(0.1, 0.8, 1, 0.5, 0.8, 1, 0, 0.5, 1), .Dim = c(3L, 3L), .Dimnames = list(c(\"(3.59,4.5]\", \"(4.5,5.4]\", \"(5.4,6.31]\"), c(\"ctrl\", \"trt1\", \"trt2\"))), \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

