expected <- eval(parse(text="4999L"));    
test(id=0, code={    
argv <- eval(parse(text="list(4999.0000000001)"));    
do.call(`as.integer`, argv);    
}, o=expected);    

