expected <- eval(parse(text="c(TRUE, TRUE, TRUE)"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(5L, 2L, 4L), c(5, 2, 4))"));   
do.call(`==`, argv);   
}, o=expected);   

