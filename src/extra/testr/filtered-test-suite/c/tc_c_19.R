expected <- eval(parse(text="c(10, 10)"));        
test(id=0, code={        
argv <- eval(parse(text="list(10L, NULL, 10)"));        
do.call(`c`, argv);        
}, o=expected);        

