expected <- eval(parse(text="list(NULL, structure(\"object\", simpleOnly = TRUE))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(\"foo\", .Dim = c(1L, 1L), .Dimnames = list(NULL, structure(\"object\", simpleOnly = TRUE))))"));      
do.call(`dimnames`, argv);      
}, o=expected);      

