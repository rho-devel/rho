expected <- eval(parse(text="structure(logical(0), .Dim = c(0L, 0L), .Dimnames = list(NULL, NULL))"));      
test(id=0, code={      
argv <- eval(parse(text="list(logical(0), structure(logical(0), .Dim = c(0L, 0L), .Dimnames = list(NULL, NULL)))"));      
do.call(`|`, argv);      
}, o=expected);      

