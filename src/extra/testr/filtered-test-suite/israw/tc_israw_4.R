expected <- eval(parse(text="FALSE"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 1, 1), .Dim = 1:3))"));    
do.call(`is.raw`, argv);    
}, o=expected);    

