expected <- eval(parse(text="quote(quote(list(NULL, c(\"time\", \"status\"))))"));   
test(id=0, code={   
argv <- eval(parse(text="list(list(quote(quote), list(NULL, c(\"time\", \"status\"))))"));   
do.call(`as.call`, argv);   
}, o=expected);   

