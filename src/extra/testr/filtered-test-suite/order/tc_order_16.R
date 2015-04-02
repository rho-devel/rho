expected <- eval(parse(text="c(2L, 1L, 3L)"));     
test(id=0, code={     
argv <- eval(parse(text="list(TRUE, FALSE, c(2L, 1L, NA))"));     
.Internal(`order`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

