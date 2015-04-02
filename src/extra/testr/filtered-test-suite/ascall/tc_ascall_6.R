expected <- eval(parse(text="quote(quote(NA))"));   
test(id=0, code={   
argv <- eval(parse(text="list(list(quote(quote), NA))"));   
do.call(`as.call`, argv);   
}, o=expected);   

