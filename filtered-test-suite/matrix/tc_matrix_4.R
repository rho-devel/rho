expected <- eval(parse(text="structure(\"foo\", .Dim = c(1L, 1L), .Dimnames = list(structure(\"object\", simpleOnly = TRUE), NULL))"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"foo\", 1L, 1, FALSE, list(structure(\"object\", simpleOnly = TRUE), NULL), FALSE, TRUE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

