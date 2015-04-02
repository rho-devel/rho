expected <- eval(parse(text="structure(list(l = 0, u = 3, n = 6L), .Names = c(\"l\", \"u\", \"n\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(0L, 3L, 5, 1, 0.75, c(1.5, 2.75), 0)"));  
.Internal(`pretty`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));  
}, o=expected);  

