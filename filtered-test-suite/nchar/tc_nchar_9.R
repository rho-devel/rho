expected <- eval(parse(text="c(3L, 3L, 2L)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"\\\"1\\\"\", \"\\\"2\\\"\", NA), \"w\", FALSE)"));     
.Internal(`nchar`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

