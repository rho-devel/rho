expected <- eval(parse(text="character(0)"));            
test(id=0, code={            
argv <- eval(parse(text="list(list(structure(character(0), .Dim = c(0L, 0L))), \"/\")"));            
.Internal(file.path(argv[[1]], argv[[2]]));            
}, o=expected);            

