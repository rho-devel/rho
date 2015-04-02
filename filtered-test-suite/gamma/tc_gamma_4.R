expected <- eval(parse(text="9.33262154439422e+157"));    
test(id=0, code={    
argv <- eval(parse(text="list(101)"));    
do.call(`gamma`, argv);    
}, o=expected);    

