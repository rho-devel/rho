expected <- eval(parse(text="c(5.4278733372119e-07, 0.000257866433233453, NA)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(5.4278733372119e-07, 0.000257866433233453, NA), .Names = c(\"x\", \"m\", \"Residuals\")), \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

