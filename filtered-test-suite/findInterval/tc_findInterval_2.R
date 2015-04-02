expected <- eval(parse(text="NA_integer_"));  
test(id=0, code={  
argv <- eval(parse(text="list(NA_real_, NA_real_, FALSE, FALSE)"));  
.Internal(findInterval(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

