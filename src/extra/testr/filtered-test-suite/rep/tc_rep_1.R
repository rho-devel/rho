expected <- eval(parse(text="c(NA, NA, NA, NA, NA, NA, NA)"));  
test(id=0, code={  
argv <- eval(parse(text="list(NA, 7)"));  
.Internal(`rep_len`(argv[[1]], argv[[2]]));  
}, o=expected);  

