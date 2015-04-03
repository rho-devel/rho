expected <- eval(parse(text="TRUE"));        
test(id=0, code={        
argv <- eval(parse(text="list(list(structure(c(-1L, -2L, -3L, -4L, -5L, -6L, -7L, -8L, -9L, -10L), .Dim = c(2L, 5L)), structure(list(V1 = 1:5, V2 = 6:10, V3 = 11:15, V4 = 16:20, V5 = 21:25), .Names = c(\"V1\", \"V2\", \"V3\", \"V4\", \"V5\"), row.names = c(NA, -5L), class = \"data.frame\")), \"any\")"));        
.Internal(`is.vector`(argv[[1]], argv[[2]]));        
}, o=expected);        

