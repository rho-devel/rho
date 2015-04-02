expected <- eval(parse(text="12"));   
test(id=0, code={   
argv <- eval(parse(text="list(FALSE, 5L, 12)"));   
.Internal(`pmax`(argv[[1]], argv[[2]], argv[[3]]));   
}, o=expected);   

