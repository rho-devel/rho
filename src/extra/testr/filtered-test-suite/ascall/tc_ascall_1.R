expected <- eval(parse(text="quote(quote(c(0.568, 1.432, -1.08, 1.08)))"));   
test(id=0, code={   
argv <- eval(parse(text="list(list(quote(quote), c(0.568, 1.432, -1.08, 1.08)))"));   
do.call(`as.call`, argv);   
}, o=expected);   

