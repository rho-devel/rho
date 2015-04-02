expected <- eval(parse(text="c(\"name\", \"title\", \"other.author\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"name\", \"title\", \"other.author\"), TRUE)"));  
.Internal(`make.names`(argv[[1]], argv[[2]]));  
}, o=expected);  

