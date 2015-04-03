expected <- eval(parse(text="c(\"NA\", \"a\", \"b\", \"c\", \"d\", \"<NA>\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"NA\", \"a\", \"b\", \"c\", \"d\", NA), 0L, \"\", 0L, TRUE)"));  
.Internal(`encodeString`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));  
}, o=expected);  

