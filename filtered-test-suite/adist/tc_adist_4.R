expected <- eval(parse(text="structure(numeric(0), .Dim = c(0L, 0L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(), list(), structure(c(1, 1, 1), .Names = c(\"insertions\", \"deletions\", \"substitutions\")), FALSE, TRUE, FALSE, FALSE, FALSE)"));  
.Internal(adist(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));  
}, o=expected);  

