expected <- eval(parse(text="c(1L, 1L, 1L, 1L)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(TRUE, TRUE, TRUE, TRUE))"));              
do.call(`abs`, argv);              
}, o=expected);              

