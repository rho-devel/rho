expected <- eval(parse(text="c(1L, 1L, 6L, 2L, 2L, 7L, 3L, 3L, 7L, 3L, 3L, 8L, 4L, 4L, 4L, 5L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(1, 1, 6, 2, 2, 7, 3, 3, 7, 3, 3, 8, 4, 4, 4, 5), .Dim = c(16L, 1L), .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\", \"11\", \"12\", \"13\", \"14\", \"15\", \"16\"), \"y\")), c(1, 2, 3, 4, 5, 6, 7, 8), NA_integer_, NULL)"));                
.Internal(match(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));                
}, o=expected);                

