expected <- eval(parse(text="Inf"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, FALSE)"));  
.Internal(lbeta(argv[[1]], argv[[2]]));  
}, o=expected);  

