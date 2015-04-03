expected <- eval(parse(text="c(3L, 3L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(\"\\\"a\\\"\", \"\\\"b\\\"\", NA, NA, NA, \"\\\"f\\\"\", \"\\\"g\\\"\", \"\\\"h\\\"\", \"\\\"i\\\"\", \"\\\"j\\\"\", \"\\\"k\\\"\", \"\\\"l\\\"\"), \"w\", FALSE)"));                
.Internal(nchar(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

