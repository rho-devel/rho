expected <- eval(parse(text="structure(c(\" 2\", \"NA\"), .Names = c(\"N:P:K\", \"Residuals\"))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(2, NA), .Names = c(\"N:P:K\", \"Residuals\")), FALSE, 5L, 0L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

