expected <- eval(parse(text="structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Dim = 10L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"1\", \"2\", \"3\", \"4\", \"5\"), .Dim = 10L), \"c\", FALSE)"));                
.Internal(nchar(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

