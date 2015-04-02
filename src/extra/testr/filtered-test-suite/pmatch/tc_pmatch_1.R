expected <- eval(parse(text="2L"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"kendall\", c(\"pearson\", \"kendall\", \"spearman\"), 0L, TRUE)"));    
.Internal(`pmatch`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));    
}, o=expected);    

