expected <- eval(parse(text="c(NA, 1L)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(NA, 1L))"));              
do.call(`abs`, argv);              
}, o=expected);              

