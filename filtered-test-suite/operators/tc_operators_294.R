expected <- eval(parse(text="c(1, 1, 1, 1, 1, 1, 1, 1, 1)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(-Inf, -2, -1, 0, 1, 2, Inf, NA, NaN), 0)"));              
do.call(`^`, argv);              
}, o=expected);              

