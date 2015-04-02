expected <- eval(parse(text="c(NA, 1, 1)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(NA, 2L, 2L))"));         
do.call(`sign`, argv);         
}, o=expected);         

