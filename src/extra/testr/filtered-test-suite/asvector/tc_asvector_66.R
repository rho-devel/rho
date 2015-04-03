expected <- eval(parse(text="c(2.2250738585072e-308, 1.79769313486232e+308)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(2.2250738585072e-308, 1.79769313486232e+308), .Names = c(\"double.xmin\", \"double.xmax\")), \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

