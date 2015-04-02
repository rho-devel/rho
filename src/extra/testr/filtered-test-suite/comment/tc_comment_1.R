expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(NULL)"));  
.Internal(comment(argv[[1]]));  
}, o=expected);  

