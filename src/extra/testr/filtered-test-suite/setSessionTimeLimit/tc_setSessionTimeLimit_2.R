expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, Inf)"));  
.Internal(setSessionTimeLimit(argv[[1]], argv[[2]]));  
}, o=expected);  

