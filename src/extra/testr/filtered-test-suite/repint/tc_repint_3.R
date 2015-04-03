expected <- eval(parse(text="c(1L, 1L, 1L, 1L)"));     
test(id=0, code={     
argv <- eval(parse(text="list(1L, 4L)"));     
.Internal(`rep.int`(argv[[1]], argv[[2]]));     
}, o=expected);     

