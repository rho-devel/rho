expected <- eval(parse(text="structure(numeric(0), .Dim = c(0L, 0L))"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 0L)))"));    
do.call(`gamma`, argv);    
}, o=expected);    

