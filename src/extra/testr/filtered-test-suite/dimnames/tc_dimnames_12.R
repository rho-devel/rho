expected <- eval(parse(text="list(NULL, c(\"Estimate\", \"Std. Error\", \"t value\", \"Pr(>|t|)\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(logical(0), .Dim = c(0L, 4L), .Dimnames = list(NULL, c(\"Estimate\", \"Std. Error\", \"t value\", \"Pr(>|t|)\"))))"));      
do.call(`dimnames`, argv);      
}, o=expected);      

