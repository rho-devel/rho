expected <- eval(parse(text="\"5 * exp(-x)\""));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(5 * exp(-x)), 60L, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

