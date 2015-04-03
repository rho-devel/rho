expected <- eval(parse(text="c(1095, 367)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(365, 365, 365, 366, 1, 0), .Dim = c(3L, 2L)), 3, 2, FALSE)"));  
.Internal(`colSums`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

