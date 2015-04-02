expected <- eval(parse(text="structure(c(-1, -1, -1, -1, -1, -1), .Dim = c(3L, 2L), .Dimnames = list(c(\"a\", \"b\", \"c\"), NULL))"));     
test(id=0, code={     
argv <- eval(parse(text="list(-1, c(3L, 2L), list(c(\"a\", \"b\", \"c\"), NULL))"));     
.Internal(`array`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

