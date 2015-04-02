expected <- eval(parse(text="TRUE"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(365, 365, 365, 366, 1, 0), .Dim = c(3L, 2L)))"));    
do.call(`is.array`, argv);    
}, o=expected);    

