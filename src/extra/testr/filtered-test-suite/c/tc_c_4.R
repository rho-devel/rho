expected <- eval(parse(text="c(1, 1, 1, 1, NA)"));        
test(id=0, code={        
argv <- eval(parse(text="list(1, 1, 1, 1, NA)"));        
do.call(`c`, argv);        
}, o=expected);        

