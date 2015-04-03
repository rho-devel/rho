expected <- eval(parse(text="c(\"class\", \"names\", \"package\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"class\", \"names\", \"package\"))"));        
do.call(`as.character`, argv);        
}, o=expected);        

