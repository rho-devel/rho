expected <- eval(parse(text="list(FALSE)"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(FALSE), 1)"));  
.Internal(getconst(argv[[1]], argv[[2]]));  
}, o=expected);  

