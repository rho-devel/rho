expected <- eval(parse(text="c(8L, 11L, 14L, 16L, 19L, 4L, 6L, 9L, 15L, NA, 7L, 10L, 20L)"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(8L, 11L, 14L, 16L, 19L, 4L, 6L, 9L, 15L, NA, 7L, 10L, 20L), \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

