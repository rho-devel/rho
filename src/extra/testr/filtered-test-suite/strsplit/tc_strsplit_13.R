expected <- eval(parse(text="list(\"x\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"x\", \"\", FALSE, FALSE, FALSE)"));    
.Internal(`strsplit`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));    
}, o=expected);    

