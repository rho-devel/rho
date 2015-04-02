expected <- eval(parse(text="c(\"1\", \"2\", NA, \"2\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(1, 2, NA, 2))"));        
do.call(`as.character`, argv);        
}, o=expected);        

