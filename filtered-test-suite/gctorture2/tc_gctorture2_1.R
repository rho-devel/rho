expected <- eval(parse(text="0L"));  
test(id=0, code={  
argv <- eval(parse(text="list(NULL, NULL, FALSE)"));  
.Internal(gctorture2(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

