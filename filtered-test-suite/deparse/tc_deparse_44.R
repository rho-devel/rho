expected <- eval(parse(text="c(\"lm(formula = 1000/MPG.city ~ Weight + Cylinders + Type + EngineSize + \", \"    DriveTrain, data = Cars93)\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(lm(formula = 1000/MPG.city ~ Weight + Cylinders + Type + EngineSize + DriveTrain, data = Cars93)), 60L, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

