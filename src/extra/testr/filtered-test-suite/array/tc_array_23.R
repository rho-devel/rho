expected <- eval(parse(text="structure(c(1L, 2L, 1L), .Dim = 3L, .Dimnames = structure(list(c(\"1\", \"2\", NA)), .Names = \"\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(1L, 2L, 1L), 3L, structure(list(c(\"1\", \"2\", NA)), .Names = \"\"))"));     
.Internal(`array`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

