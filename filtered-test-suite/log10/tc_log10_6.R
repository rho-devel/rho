expected <- eval(parse(text="c(1, 2, 3, 4, 5)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(10, 100, 1000, 10000, 1e+05))"));         
do.call(`log10`, argv);         
}, o=expected);         

