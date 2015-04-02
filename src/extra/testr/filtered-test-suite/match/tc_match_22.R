expected <- eval(parse(text="c(0L, 0L, 0L, 3L, 4L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(NA, NA, 3, 4, 5), c(NA, NA, 4, 5), 0L, NA)"));                
.Internal(match(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));                
}, o=expected);                

