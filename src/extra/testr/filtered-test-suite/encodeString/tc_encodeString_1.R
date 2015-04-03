expected <- eval(parse(text="c(\"\\\"1\\\"\", \"\\\"2\\\"\", NA)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"1\", \"2\", NA), 0L, \"\\\"\", 0L, FALSE)"));  
.Internal(`encodeString`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));  
}, o=expected);  

