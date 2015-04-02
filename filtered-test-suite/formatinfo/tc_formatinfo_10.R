expected <- eval(parse(text="c(11L, 8L, 0L)"));         
test(id=0, code={         
argv <- eval(parse(text="list(31.4159265358979, NULL, 8)"));         
.Internal(format.info(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

