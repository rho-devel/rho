expected <- eval(parse(text="numeric(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(logical(0), logical(0), 1)"));  
.Internal(besselI(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

