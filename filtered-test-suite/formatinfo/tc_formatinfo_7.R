expected <- eval(parse(text="c(12L, 6L, 1L)"));         
test(id=0, code={         
argv <- eval(parse(text="list(3.14159265358979e-10, NULL, 8)"));         
.Internal(format.info(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

