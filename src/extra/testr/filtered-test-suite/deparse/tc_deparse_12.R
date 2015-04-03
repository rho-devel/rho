expected <- eval(parse(text="\"t1 - 4\""));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(t1 - 4), 60L, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

