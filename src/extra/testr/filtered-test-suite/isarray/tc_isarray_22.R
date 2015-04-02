expected <- eval(parse(text="TRUE"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(1:12, .Dim = 3:4, .Dimnames = list(c(\"A\", \"B\", \"C\"), c(\"D\", \"E\", \"F\", \"G\"))))"));    
do.call(`is.array`, argv);    
}, o=expected);    

