expected <- eval(parse(text="structure(c(1259, 1360, 845, 1053, 719, 774, 390, 413), .Dim = c(2L, 4L))"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(1259, 845, 719, 390, 1360, 1053, 774, 413), 2, 1, TRUE, NULL, FALSE, TRUE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

