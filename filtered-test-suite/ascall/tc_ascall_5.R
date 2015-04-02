expected <- eval(parse(text="quote(quote(80L))"));   
test(id=0, code={   
argv <- eval(parse(text="list(list(quote(quote), 80L))"));   
do.call(`as.call`, argv);   
}, o=expected);   

