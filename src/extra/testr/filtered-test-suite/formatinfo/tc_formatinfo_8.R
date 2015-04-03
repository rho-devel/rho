expected <- eval(parse(text="c(5L, 0L, 1L)"));         
test(id=0, code={         
argv <- eval(parse(text="list(1e+08, NULL, 0L)"));         
.Internal(format.info(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

