expected <- eval(parse(text="c(\"y ~ ((g1) * exp((log(g2/g1)) * (1 - exp(-k * (x - Ta)))/(1 - \", \"    exp(-k * (Tb - Ta)))))\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(y ~ ((g1) * exp((log(g2/g1)) * (1 - exp(-k * (x - Ta)))/(1 - exp(-k * (Tb - Ta)))))), 60L, TRUE, 69, -1L)"));     
.Internal(`deparse`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));     
}, o=expected);     

