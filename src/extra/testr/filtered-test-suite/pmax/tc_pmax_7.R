expected <- eval(parse(text="c(15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L)"));   
test(id=0, code={   
argv <- eval(parse(text="list(FALSE, 1L, c(15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L))"));   
.Internal(`pmax`(argv[[1]], argv[[2]], argv[[3]]));   
}, o=expected);   

