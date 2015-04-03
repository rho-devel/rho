expected <- eval(parse(text="c(1, 2, 3)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(1L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 3L), .Dim = c(3L, 3L)), 3, 3, FALSE)"));  
.Internal(`colSums`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

