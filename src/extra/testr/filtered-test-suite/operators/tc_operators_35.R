expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(3, 1, 2, 2, 3.2, -1, 1, 3.2, 4, 3, 3.2, 3.2, 202, 241, 243), 0)"));         
do.call(`!=`, argv);         
}, o=expected);         

