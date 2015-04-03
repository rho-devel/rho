expected <- eval(parse(text="list(\"Keywords:  utilities \")"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"Keywords:  utilities \", \"\\n[ \\t\\n]*\\n\", FALSE, TRUE, TRUE)"));    
.Internal(`strsplit`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));    
}, o=expected);    

