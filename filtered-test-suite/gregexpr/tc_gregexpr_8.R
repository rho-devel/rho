expected <- eval(parse(text="list(structure(1:3, match.length = c(0L, 0L, 0L), useBytes = TRUE))"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"\", \"abc\", FALSE, TRUE, FALSE, FALSE)"));  
.Internal(`gregexpr`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));  
}, o=expected);  

