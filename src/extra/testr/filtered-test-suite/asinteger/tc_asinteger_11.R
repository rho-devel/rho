expected <- eval(parse(text="1L"));    
test(id=0, code={    
argv <- eval(parse(text="list(TRUE)"));    
do.call(`as.integer`, argv);    
}, o=expected);    

