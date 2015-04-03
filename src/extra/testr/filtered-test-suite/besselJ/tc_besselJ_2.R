expected <- eval(parse(text="1"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, FALSE)"));  
.Internal(besselJ(argv[[1]], argv[[2]]));  
}, o=expected);  

