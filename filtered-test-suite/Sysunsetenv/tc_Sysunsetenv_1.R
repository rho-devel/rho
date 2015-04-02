expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"_R_NS_LOAD_\")"));  
.Internal(`Sys.unsetenv`(argv[[1]]));  
}, o=expected);  

