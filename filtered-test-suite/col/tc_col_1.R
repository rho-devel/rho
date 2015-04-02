expected <- eval(parse(text="structure(c(1L, 1L, 2L, 2L), .Dim = c(2L, 2L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(2L, 2L))"));  
.Internal(`col`(argv[[1]]));  
}, o=expected);  

