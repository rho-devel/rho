expected <- eval(parse(text="c(1L, 2L, 0L)"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(10, class = c(\"a\", \"b\")), c(\"a\", \"b\", \"c\"), TRUE)"));                   
.Internal(inherits(argv[[1]], argv[[2]], argv[[3]]));                   
}, o=expected);                   

