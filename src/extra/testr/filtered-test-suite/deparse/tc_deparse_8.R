expected <- eval(parse(text="\"tt <- table(c(rep(0, 7), rep(1, 4), rep(5, 3)))\""));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(tt <- table(c(rep(0, 7), rep(1, 4), rep(5, 3)))), 60L, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

