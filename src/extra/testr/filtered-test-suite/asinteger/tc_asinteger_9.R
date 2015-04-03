expected <- eval(parse(text="c(1L, NA, 0L)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(\"1\", NA, \"0\"))"));              
do.call(`as.integer`, argv);              
}, o=expected);              

