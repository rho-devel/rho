expected <- eval(parse(text="c(3L, 2147483647L)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(\"3\", \"14159265358979\"))"));              
do.call(`as.integer`, argv);              
}, o=expected);              

