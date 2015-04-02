expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(1L, 7L))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(1:7, .Dim = c(1L, 7L)))"));      
do.call(`is.finite`, argv);      
}, o=expected);      

