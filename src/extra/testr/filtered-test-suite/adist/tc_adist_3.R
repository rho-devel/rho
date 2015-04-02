expected <- eval(parse(text="structure(1, .Dim = c(1L, 1L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"lasy\", \"1 lazy 2\", c(1L, 1L, 1L), FALSE, TRUE, TRUE, FALSE, FALSE)"));  
.Internal(adist(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));  
}, o=expected);  

