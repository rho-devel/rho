expected <- eval(parse(text="quote(quote(FALSE))"));   
test(id=0, code={   
argv <- eval(parse(text="list(list(quote(quote), FALSE))"));   
do.call(`as.call`, argv);   
}, o=expected);   

