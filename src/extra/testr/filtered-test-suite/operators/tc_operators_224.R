expected <- eval(parse(text="c(5, 6, 7, 8, 9)"));    
test(id=0, code={    
argv <- eval(parse(text="list(4, 1:5)"));    
do.call(`+`, argv);    
}, o=expected);    

