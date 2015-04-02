expected <- eval(parse(text="c(369.430769230769, 4.99999999999983)"));        
test(id=0, code={        
argv <- eval(parse(text="list(369.430769230769, 4.99999999999983)"));        
do.call(`c`, argv);        
}, o=expected);        

