expected <- eval(parse(text="NA_integer_"));     
test(id=0, code={     
argv <- eval(parse(text="list(NA_integer_, 1L)"));     
.Internal(`rep.int`(argv[[1]], argv[[2]]));     
}, o=expected);     

