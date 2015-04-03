expected <- eval(parse(text="3L"));         
test(id=0, code={         
argv <- eval(parse(text="list(712L, NULL, 0L)"));         
.Internal(format.info(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

