expected <- eval(parse(text="c(0.1, 1e+60)"));        
test(id=0, code={        
argv <- eval(parse(text="list(0.1, 1e+60)"));        
do.call(`c`, argv);        
}, o=expected);        

