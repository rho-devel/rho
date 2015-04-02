expected <- eval(parse(text="c(\"\\\"1\\\"\", \"\\\"2\\\"\", NA)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"\\\"1\\\"\", \"\\\"2\\\"\", NA), 128)"));  
.Internal(`strtrim`(argv[[1]], argv[[2]]));  
}, o=expected);  

