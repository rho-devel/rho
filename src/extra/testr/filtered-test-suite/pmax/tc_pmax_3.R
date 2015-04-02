expected <- eval(parse(text="c(6L, 5L, 5L, 5L, 5L)"));            
test(id=0, code={            
argv <- eval(parse(text="list(FALSE, c(0L, 1L, 1L, 1L, 2L), 5L, c(6L, 5L, 5L, 5L, 4L))"));            
.Internal(pmax(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));            
}, o=expected);            

