expected <- eval(parse(text="c(TRUE, TRUE)"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"([[:digit:]]+[.-]){1,}[[:digit:]]+\", c(\"1.0\", \"1.0\"), FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)"));    
.Internal(`grepl`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));    
}, o=expected);    

