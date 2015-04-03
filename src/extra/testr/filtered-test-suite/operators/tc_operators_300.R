expected <- eval(parse(text="c(0, 0, 0)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(1.1, 2, Inf), -Inf)"));              
do.call(`^`, argv);              
}, o=expected);              

