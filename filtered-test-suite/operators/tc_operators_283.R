expected <- eval(parse(text="c(Inf+0i, Inf+0i, Inf+0i, 1+0i, 0+0i, 0+0i, 0+0i)"));              
test(id=0, code={              
argv <- eval(parse(text="list(0+0i, -3:3)"));              
do.call(`^`, argv);              
}, o=expected);              

