expected <- eval(parse(text="structure(1:25, .Dim = c(5L, 5L))"));    
test(id=0, code={    
argv <- eval(parse(text="list(1:25, 5, 5, FALSE, NULL, FALSE, FALSE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

