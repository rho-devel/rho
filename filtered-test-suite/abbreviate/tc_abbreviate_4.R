expected <- eval(parse(text="c(\"(Intr)\", \"s(*p*T\", \"c(*p*T\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"(Intercept)\", \"sin(2 * pi * Time)\", \"cos(2 * pi * Time)\"), 6, TRUE)"));     
.Internal(abbreviate(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

