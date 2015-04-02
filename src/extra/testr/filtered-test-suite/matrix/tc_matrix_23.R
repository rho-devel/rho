expected <- eval(parse(text="structure(character(0), .Dim = c(0L, 17L))"));    
test(id=0, code={    
argv <- eval(parse(text="list(character(0), 0L, 17L, FALSE, NULL, FALSE, FALSE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

