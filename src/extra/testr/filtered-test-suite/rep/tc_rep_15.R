expected <- eval(parse(text="c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(1:5, 15)"));  
.Internal(`rep_len`(argv[[1]], argv[[2]]));  
}, o=expected);  

