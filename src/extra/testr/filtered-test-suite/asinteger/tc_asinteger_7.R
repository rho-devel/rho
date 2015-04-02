expected <- eval(parse(text="c(7L, 20L, 0L, 1L)"));              
test(id=0, code={              
argv <- eval(parse(text="list(list(7L, 20, 0L, 1))"));              
do.call(`as.integer`, argv);              
}, o=expected);              

