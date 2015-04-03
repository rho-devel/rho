expected <- eval(parse(text="\"2014-03-\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"2014-03-17 14:47:20\", 8)"));  
.Internal(`strtrim`(argv[[1]], argv[[2]]));  
}, o=expected);  

