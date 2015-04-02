expected <- eval(parse(text="structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = c(2L, 5L))"));    
test(id=0, code={    
argv <- eval(parse(text="list(NA, 2, 5, FALSE, NULL, FALSE, FALSE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

