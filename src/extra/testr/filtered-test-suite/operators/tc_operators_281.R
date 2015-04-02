expected <- eval(parse(text="c(9, 4, 1, 0, 1, 4, 9, 16)"));              
test(id=0, code={              
argv <- eval(parse(text="list(-3:4, 2)"));              
do.call(`^`, argv);              
}, o=expected);              

