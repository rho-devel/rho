expected <- eval(parse(text="0.02"));    
test(id=0, code={    
argv <- eval(parse(text="list(0.02)"));    
do.call(`+`, argv);    
}, o=expected);    

