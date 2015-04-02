expected <- eval(parse(text="1"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, FALSE, 1)"));  
.Internal(besselI(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

