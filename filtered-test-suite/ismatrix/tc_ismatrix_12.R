expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(-15.8396536770559, 0.267020886067525, -10.0516337591148, 7.62751967585832), .Dim = c(2L, 2L), .Dimnames = list(c(\"1\", \"3\"), c(\"(Intercept)\", \"TempLow\"))))"));      
do.call(`is.matrix`, argv);      
}, o=expected);      

