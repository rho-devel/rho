expected <- eval(parse(text="\"\""));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 0L)))"));  
.Internal(environmentName(argv[[1]]));  
}, o=expected);  

