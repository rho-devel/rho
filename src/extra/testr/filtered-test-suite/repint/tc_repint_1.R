expected <- eval(parse(text="c(1, 1, 1, 1, 1, 1)"));     
test(id=0, code={     
argv <- eval(parse(text="list(1, 6)"));     
.Internal(`rep.int`(argv[[1]], argv[[2]]));     
}, o=expected);     

