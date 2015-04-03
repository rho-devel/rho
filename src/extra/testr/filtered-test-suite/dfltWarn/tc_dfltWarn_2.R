expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"bessel_y(2,nu=288.12): precision lost in result\", quote(besselY(2, nu = nu <- seq(3, 300, len = 51))))"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

