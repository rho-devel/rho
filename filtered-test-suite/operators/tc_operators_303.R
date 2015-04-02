expected <- eval(parse(text="structure(c(TRUE, FALSE, TRUE, TRUE), .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(TRUE, FALSE, TRUE, TRUE), .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL)), structure(c(FALSE, FALSE, FALSE, FALSE), .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL)))"));      
do.call(`|`, argv);      
}, o=expected);      

