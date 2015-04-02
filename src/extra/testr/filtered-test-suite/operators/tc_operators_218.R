expected <- eval(parse(text="3+5i"));    
test(id=0, code={    
argv <- eval(parse(text="list(3, 0+5i)"));    
do.call(`+`, argv);    
}, o=expected);    

