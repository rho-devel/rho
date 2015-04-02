expected <- eval(parse(text="structure(3, .Dim = c(1L, 1L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(c(107L, 105L, 116L, 116L, 101L, 110L)), list(c(115L, 105L, 116L, 116L, 105L, 110L, 103L)), structure(c(1, 1, 1), .Names = c(\"insertions\", \"deletions\", \"substitutions\")), FALSE, TRUE, FALSE, FALSE, FALSE)"));  
.Internal(adist(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));  
}, o=expected);  

