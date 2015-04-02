expected <- eval(parse(text="5L"));         
test(id=0, code={         
argv <- eval(parse(text="list(FALSE, NULL, 0L)"));         
.Internal(format.info(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

