expected <- eval(parse(text="\"\""));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE)"));  
.Internal(environmentName(argv[[1]]));  
}, o=expected);  

