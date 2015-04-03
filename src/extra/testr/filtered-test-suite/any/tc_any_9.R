expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(4L, 3L), .Dimnames = list(c(\"<none>\", \"Hair:Eye\", \"Hair:Sex\", \"Eye:Sex\"), c(\"Df\", \"Deviance\", \"AIC\"))))"));      
do.call(`any`, argv);      
}, o=expected);      

