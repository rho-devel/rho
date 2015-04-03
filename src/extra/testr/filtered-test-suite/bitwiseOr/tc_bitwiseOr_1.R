expected <- eval(parse(text="15L"));  
test(id=0, code={  
argv <- eval(parse(text="list(15L, 7L)"));  
.Internal(bitwiseOr(argv[[1]], argv[[2]]));  
}, o=expected);  

