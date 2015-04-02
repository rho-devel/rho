expected <- eval(parse(text="511L"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"0777\", 8L)"));  
.Internal(`strtoi`(argv[[1]], argv[[2]]));  
}, o=expected);  

