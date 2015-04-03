expected <- eval(parse(text="\"UTF-8\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"UTF-8\")"));  
.Internal(`toupper`(argv[[1]]));  
}, o=expected);  

