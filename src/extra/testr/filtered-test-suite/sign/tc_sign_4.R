expected <- eval(parse(text="c(1, 1)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(2, 1.5))"));         
do.call(`sign`, argv);         
}, o=expected);         

