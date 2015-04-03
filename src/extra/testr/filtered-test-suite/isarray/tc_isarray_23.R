expected <- eval(parse(text="FALSE"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(0.568, 1.432, -1.08, 1.08))"));    
do.call(`is.array`, argv);    
}, o=expected);    

