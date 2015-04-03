expected <- eval(parse(text="0"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(1, 1, 3, 3), .Dim = c(2L, 2L)), \"O\")"));    
.Internal(`La_dgecon`(argv[[1]], argv[[2]]));    
}, o=expected);    

