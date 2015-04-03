expected <- eval(parse(text="\"1 lazy\""));    
test(id=0, code={    
argv <- eval(parse(text="list(\"laysy\", c(\"1 lazy\", \"1\", \"1 LAZY\"), FALSE, TRUE, c(1L, 1L, 1L), c(2, NA, NA, NA, NA), FALSE, TRUE)"));    
.Internal(agrep(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));    
}, o=expected);    

