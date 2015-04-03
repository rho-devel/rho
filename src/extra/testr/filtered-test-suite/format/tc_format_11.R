expected <- eval(parse(text="structure(c(\"1\", \"2\", \"1\"), .Dim = 3L, .Dimnames = structure(list(c(\"1\", \"2\", NA)), .Names = \"\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1L, 2L, 1L), .Dim = 3L, .Dimnames = structure(list(c(\"1\", \"2\", NA)), .Names = \"\")), FALSE, 7L, 0L, NULL, 3L, TRUE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

