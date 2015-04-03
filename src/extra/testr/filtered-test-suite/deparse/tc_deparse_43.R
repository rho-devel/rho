expected <- eval(parse(text="c(\"glm(formula = y ~ x, family = poisson(identity), start = c(1, \", \"    0))\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(glm(formula = y ~ x, family = poisson(identity), start = c(1, 0))), 60L, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

