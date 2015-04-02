expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)"));     
test(id=0, code={     
argv <- eval(parse(text="list(TRUE, 6L)"));     
.Internal(`rep.int`(argv[[1]], argv[[2]]));     
}, o=expected);     

