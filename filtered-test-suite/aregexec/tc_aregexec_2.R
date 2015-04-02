expected <- eval(parse(text="list(structure(c(3L, 3L, 5L), match.length = c(4L, 2L, 2L)), structure(-1L, match.length = -1L), structure(-1L, match.length = -1L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"(lay)(sy)\", c(\"1 lazy\", \"1\", \"1 LAZY\"), c(2, NA, NA, NA, NA), c(1L, 1L, 1L), FALSE, FALSE, FALSE)"));  
.Internal(aregexec(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));  
}, o=expected);  

