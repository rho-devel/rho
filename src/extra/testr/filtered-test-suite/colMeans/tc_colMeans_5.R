expected <- eval(parse(text="c(1, 0.5, 0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(2, 1, 0, 1, 0, NA, NA, NA, 0), .Dim = c(3L, 3L)), 3, 3, TRUE)"));  
.Internal(`colMeans`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

