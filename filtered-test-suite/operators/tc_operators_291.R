expected <- eval(parse(text="c(NaN, NaN, NaN)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(-Inf, -2, -1), 0.5)"));              
do.call(`^`, argv);              
}, o=expected);              

