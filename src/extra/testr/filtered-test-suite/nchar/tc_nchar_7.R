expected <- eval(parse(text="c(2L, 3L, 2L)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"x1\", \"x.2\", \"x3\"), \"bytes\", FALSE)"));     
.Internal(`nchar`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

