expected <- eval(parse(text="structure(c(NA_integer_, NA_integer_), .Dim = 1:2, .Dimnames = list(\"x\", c(\"Estimate\", \"Std. Error\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(NA, NA), .Dim = 1:2, .Dimnames = list(\"x\", c(\"Estimate\", \"Std. Error\"))))"));      
do.call(`abs`, argv);      
}, o=expected);      

