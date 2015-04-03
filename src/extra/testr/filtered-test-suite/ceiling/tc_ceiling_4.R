expected <- eval(parse(text="c(1, 6, 11, 16, 20)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(1, 5.5, 10.5, 15.5, 20))"));         
do.call(`ceiling`, argv);         
}, o=expected);         

