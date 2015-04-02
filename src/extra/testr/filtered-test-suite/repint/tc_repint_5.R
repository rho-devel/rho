expected <- eval(parse(text="logical(0)"));     
test(id=0, code={     
argv <- eval(parse(text="list(FALSE, 0L)"));     
.Internal(`rep.int`(argv[[1]], argv[[2]]));     
}, o=expected);     

