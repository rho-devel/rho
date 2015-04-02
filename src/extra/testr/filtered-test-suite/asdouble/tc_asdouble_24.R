expected <- eval(parse(text="c(NA, NA, 1, 2, 3, 4, 5, 6, 7, 8, 9)"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(NA, NA, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L))"));               
do.call(`as.double`, argv);               
}, o=expected);               

