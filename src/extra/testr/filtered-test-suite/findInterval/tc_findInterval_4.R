expected <- eval(parse(text="c(0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(5, 10, 15), c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18), FALSE, FALSE)"));  
.Internal(findInterval(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

