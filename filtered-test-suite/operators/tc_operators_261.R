expected <- eval(parse(text="c(TRUE, TRUE, NA)"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(\"1\", \"2\", NA), c(\"1\", \"2\", NA))"));   
do.call(`==`, argv);   
}, o=expected);   

