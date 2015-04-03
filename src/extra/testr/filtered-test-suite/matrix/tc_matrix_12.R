expected <- eval(parse(text="structure(1:4, .Dim = c(2L, 2L))"));    
test(id=0, code={    
argv <- eval(parse(text="list(1:4, 1, 2, FALSE, NULL, TRUE, FALSE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

