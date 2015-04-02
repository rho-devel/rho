expected <- eval(parse(text="c(2L, 1L, 1L, 1L)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(0.00316901674455053, 0.000313731190323184, 2.12051012154177e-05, 0.000158772845963692), c(0, 0.001, 0.01, 0.05, 0.1, 1), TRUE, TRUE)"));       
.Internal(bincode(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));       
}, o=expected);       

