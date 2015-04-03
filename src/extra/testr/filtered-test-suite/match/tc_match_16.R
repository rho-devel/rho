expected <- eval(parse(text="c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(1L, 2L, 4L, 13L, 14L, 15L, 16L, 17L, 18L, 23L), c(23L, 28L), 0L, NULL)"));                
.Internal(match(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));                
}, o=expected);                

