expected <- eval(parse(text="structure(c(NA, NA, NA, \"a\", NA, NA, \"b\", \"d\", NA, \"10\", \"12\", \"14\"), .Dim = 3:4)"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(NA, \"a\", \"b\", \"10\", NA, NA, \"d\", \"12\", NA, NA, NA, \"14\"), 1, 4, TRUE, NULL, TRUE, FALSE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

