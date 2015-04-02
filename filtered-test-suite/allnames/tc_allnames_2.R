expected <- eval(parse(text="character(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(logical(0), logical(0), -1L, TRUE)"));   
.Internal(all.names(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));   
}, o=expected);   

