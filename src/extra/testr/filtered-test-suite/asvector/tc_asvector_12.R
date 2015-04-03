expected <- eval(parse(text="c(0.164593338447767, 0.182090654313858, NA, 0.484947927602608)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(0.164593338447767, 0.182090654313858, NA, 0.484947927602608), .Names = c(\"(Intercept)\", \"x1\", \"x2\", \"x3\")), \"any\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

