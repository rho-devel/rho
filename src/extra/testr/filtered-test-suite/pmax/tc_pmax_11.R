expected <- eval(parse(text="c(7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L)"));   
test(id=0, code={   
argv <- eval(parse(text="list(FALSE, c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L), 7L, c(7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 6L))"));   
.Internal(`pmax`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));   
}, o=expected);   

