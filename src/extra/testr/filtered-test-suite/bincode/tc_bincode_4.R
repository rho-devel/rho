expected <- eval(parse(text="c(NA, 4L, 5L)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(NA, 0.0654707112145736, 0.999999999999999), c(0, 0.001, 0.01, 0.05, 0.1, 1), TRUE, TRUE)"));       
.Internal(bincode(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));       
}, o=expected);       

