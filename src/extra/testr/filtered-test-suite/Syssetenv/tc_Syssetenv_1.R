expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"_R_NS_LOAD_\", \"Matrix\")"));  
.Internal(`Sys.setenv`(argv[[1]], argv[[2]]));  
}, o=expected);  

