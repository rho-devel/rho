expected <- eval(parse(text="character(0)"));        
test(id=0, code={        
argv <- eval(parse(text="list(quote(cbind(X, M) ~ 1), \"term.labels\")"));        
do.call(`attr`, argv);        
}, o=expected);        

