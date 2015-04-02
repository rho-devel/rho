expected <- eval(parse(text="numeric(0)"));     
test(id=0, code={     
argv <- eval(parse(text="list(numeric(0), TRUE, 0L)"));     
.Internal(`setS4Object`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

