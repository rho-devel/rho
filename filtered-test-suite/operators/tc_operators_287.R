expected <- eval(parse(text="c(1, 1, 1, 1, 1, 1, 1, 1, 1)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(NA, -2L, -1L, 0L, 1L, 2L, NA, NA, NA), 0L)"));              
do.call(`^`, argv);              
}, o=expected);              

