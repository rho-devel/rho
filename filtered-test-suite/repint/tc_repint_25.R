expected <- eval(parse(text="c(NA, NA, NA, NA, NA)"));     
test(id=0, code={     
argv <- eval(parse(text="list(NA, 5L)"));     
.Internal(`rep.int`(argv[[1]], argv[[2]]));     
}, o=expected);     

