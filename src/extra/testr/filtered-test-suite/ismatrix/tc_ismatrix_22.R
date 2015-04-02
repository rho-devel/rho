expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(NA, NA, NA, NA), .Dim = c(1L, 4L), .Dimnames = list(\"x\", c(\"Estimate\", \"Std. Error\", \"t value\", \"Pr(>|t|)\"))))"));      
do.call(`is.matrix`, argv);      
}, o=expected);      

