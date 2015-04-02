expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(logical(0), .Dim = c(10L, 0L)))"));      
do.call(`is.matrix`, argv);      
}, o=expected);      

