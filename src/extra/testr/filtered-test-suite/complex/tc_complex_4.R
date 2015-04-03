expected <- eval(parse(text="NA_complex_"));  
test(id=0, code={  
argv <- eval(parse(text="list(0L, NA_real_, NA_real_)"));  
.Internal(complex(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

