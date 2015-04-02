expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(1, 1, 10, 1, 1, 10, 10), 0)"));         
do.call(`!=`, argv);         
}, o=expected);         

