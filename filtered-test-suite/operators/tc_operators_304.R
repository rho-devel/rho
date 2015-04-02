expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE), .Dim = c(2L, 2L))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE), .Dim = c(2L, 2L)), structure(c(FALSE, FALSE, FALSE, FALSE), .Dim = c(2L, 2L)))"));      
do.call(`|`, argv);      
}, o=expected);      

