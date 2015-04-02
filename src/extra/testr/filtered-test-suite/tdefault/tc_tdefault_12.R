expected <- eval(parse(text="structure(\"foo\", .Dim = c(1L, 1L), .Dimnames = list(NULL, structure(\"object\", simpleOnly = TRUE)))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(\"foo\", .Dim = c(1L, 1L), .Dimnames = list(structure(\"object\", simpleOnly = TRUE), NULL)))"));  
.Internal(`t.default`(argv[[1]]));  
}, o=expected);  

