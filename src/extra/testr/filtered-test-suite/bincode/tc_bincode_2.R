expected <- eval(parse(text="c(1L, 1L, 1L)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(8.70599232813489e-06, 7.24187268717448e-10, 7.84878459581784e-14), c(0, 0.001, 0.01, 0.05, 0.1, 1), TRUE, TRUE)"));       
.Internal(bincode(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));       
}, o=expected);       

