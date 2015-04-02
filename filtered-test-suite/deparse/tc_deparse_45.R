expected <- eval(parse(text="\"0.333333333333333\""));     
test(id=0, code={     
argv <- eval(parse(text="list(0.333333333333333, 60L, FALSE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

