expected <- eval(parse(text="list(structure(1L, match.length = 5L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"FALSE\", \"FALSE\", c(0.1, NA, NA, NA, NA), c(1L, 1L, 1L), FALSE, FALSE, FALSE)"));  
.Internal(aregexec(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));  
}, o=expected);  

