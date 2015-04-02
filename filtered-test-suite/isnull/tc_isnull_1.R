expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(\"a\", \"b\", \"c\"))"));   
do.call(`is.null`, argv);   
}, o=expected);   

