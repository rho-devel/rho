expected <- eval(parse(text="c(0, 1, 1, 0, 0, 1, 1, 0, 0)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(0, 1, 1, 2, 2, 3, 3, 4, 4), 2L)"));              
do.call(`%%`, argv);              
}, o=expected);              

