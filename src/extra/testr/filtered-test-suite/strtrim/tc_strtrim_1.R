expected <- eval(parse(text="c(\"\\\"time\\\"\", \"\\\"status\\\"\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"\\\"time\\\"\", \"\\\"status\\\"\"), 128)"));  
.Internal(`strtrim`(argv[[1]], argv[[2]]));  
}, o=expected);  

