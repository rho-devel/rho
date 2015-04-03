expected <- eval(parse(text="numeric(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(logical(0), logical(0))"));  
.Internal(beta(argv[[1]], argv[[2]]));  
}, o=expected);  

