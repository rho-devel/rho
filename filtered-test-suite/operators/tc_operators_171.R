expected <- eval(parse(text="c(1, 0, 0, 0, 0, 0, 1, 1, 1)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(-1, 0, 0, 0, 0, 0, 1, 1, 1), 2L)"));              
do.call(`%%`, argv);              
}, o=expected);              

