expected <- eval(parse(text="FALSE"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(\"1\", \"2\", NA))"));    
do.call(`is.language`, argv);    
}, o=expected);    

