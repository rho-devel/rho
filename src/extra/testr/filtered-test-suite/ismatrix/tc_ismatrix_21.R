expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(-15.7116658409483, 0.267197739695975, -7.51681521806951, 7.8485143735526), .Dim = c(2L, 2L), .Dimnames = list(c(\"1\", \"3\"), c(\"(Intercept)\", \"M.userY\"))))"));      
do.call(`is.matrix`, argv);      
}, o=expected);      

