expected <- eval(parse(text="\"show\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"show\")"));  
.Internal(`tolower`(argv[[1]]));  
}, o=expected);  

