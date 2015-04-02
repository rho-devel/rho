expected <- eval(parse(text="c(2, 3, 4, 5, 6, 7, 12, 22)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(2, 3, 4, 5, 6, 7, 12, 22), 8L)"));  
.Internal(`rep_len`(argv[[1]], argv[[2]]));  
}, o=expected);  

