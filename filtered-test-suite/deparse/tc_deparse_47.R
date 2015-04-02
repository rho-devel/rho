expected <- eval(parse(text="c(\"glm(formula = cbind(X, M) ~ M.user + Temp + M.user:Temp, family = binomial, \", \"    data = detg1)\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(glm(formula = cbind(X, M) ~ M.user + Temp + M.user:Temp, family = binomial, data = detg1)), 60L, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

