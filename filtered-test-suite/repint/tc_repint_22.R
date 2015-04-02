expected <- eval(parse(text="c(NA_real_, NA_real_, NA_real_, NA_real_)"));     
test(id=0, code={     
argv <- eval(parse(text="list(NA_real_, 4L)"));     
.Internal(`rep.int`(argv[[1]], argv[[2]]));     
}, o=expected);     

