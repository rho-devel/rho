expected <- eval(parse(text="c(TRUE, TRUE)"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(-Inf, Inf), c(-Inf, Inf))"));   
do.call(`==`, argv);   
}, o=expected);   

