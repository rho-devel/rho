expected <- eval(parse(text="Inf"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, FALSE, 1)"));  
.Internal(besselK(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

