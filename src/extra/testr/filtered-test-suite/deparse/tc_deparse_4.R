expected <- eval(parse(text="\"unclass(x)\""));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(unclass(x)), 500, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

