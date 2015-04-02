expected <- eval(parse(text="\"abc\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"abc\", \"UTF-8\")"));  
.Internal(`setEncoding`(argv[[1]], argv[[2]]));  
}, o=expected);  

