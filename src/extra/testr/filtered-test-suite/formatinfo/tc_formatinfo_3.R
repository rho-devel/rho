expected <- eval(parse(text="c(4L, 0L, 0L)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(Inf, -Inf), NULL, 0L)"));         
.Internal(format.info(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

