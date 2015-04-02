expected <- structure(c(0, NaN, 0, 4.94065645841247e-324), class = "integer64")
test(id=0, code={
argv <- list(structure(c(0, NaN, 0, 4.94065645841247e-324), class = "integer64"))
do.call('signif', argv);
},  o = expected);

