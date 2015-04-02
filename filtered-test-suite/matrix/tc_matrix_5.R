expected <- eval(parse(text="structure(c(1, 2, 3, 0, 10, NA), .Dim = c(3L, 2L))"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(1, 2, 3, 0, 10, NA), 3, 2, FALSE, NULL, FALSE, FALSE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

