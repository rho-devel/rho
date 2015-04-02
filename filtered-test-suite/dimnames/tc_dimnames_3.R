expected <- eval(parse(text="NULL"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE), .Dim = c(3L, 1L)))"));      
do.call(`dimnames`, argv);      
}, o=expected);      

