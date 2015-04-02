expected <- eval(parse(text="c(5L, 2L, 6L, 2L, 9L, 11L, 3L, 10L, 9L, 5L, 9L)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5), .Names = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\", \"k\")), 11L, \"max\")"));        
.Internal(rank(argv[[1]], argv[[2]], argv[[3]]));        
}, o=expected);        

