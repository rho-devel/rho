expected <- eval(parse(text="c(-1, -1, -1)"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(-0.783587745879035, -0.739712343519063, -0.314304892261569))"));    
do.call(`floor`, argv);    
}, o=expected);    

