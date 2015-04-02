expected <- eval(parse(text="structure(c(1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 1), .Dim = c(5L, 5L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(1L, 2L, 3L, 4L, 1L), .Dim = 5L), 5L, 5L)"));  
.Internal(`diag`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

