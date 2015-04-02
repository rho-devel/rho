expected <- eval(parse(text="\"lm(formula = y ~ x1 + x2 + x3)\""));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(lm(formula = y ~ x1 + x2 + x3)), 60L, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

