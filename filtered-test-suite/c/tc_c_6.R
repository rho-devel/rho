expected <- eval(parse(text="c(TRUE, TRUE, NA)"));        
test(id=0, code={        
argv <- eval(parse(text="list(TRUE, TRUE, NA)"));        
do.call(`c`, argv);        
}, o=expected);        

