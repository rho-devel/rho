expected <- eval(parse(text="c(0, -0.125, -1)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(-Inf, -2, -1), -3)"));              
do.call(`^`, argv);              
}, o=expected);              

