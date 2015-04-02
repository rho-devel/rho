expected <- eval(parse(text="integer(0)"));    
test(id=0, code={    
argv <- eval(parse(text="list(character(0))"));    
do.call(`as.integer`, argv);    
}, o=expected);    

