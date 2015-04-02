expected <- eval(parse(text="c(-0.1, 0.1)"));        
test(id=0, code={        
argv <- eval(parse(text="list(-0.1, 0.1)"));        
do.call(`c`, argv);        
}, o=expected);        

