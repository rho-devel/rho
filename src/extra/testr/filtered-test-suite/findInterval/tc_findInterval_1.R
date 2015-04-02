expected <- eval(parse(text="c(3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1, 2, 3, 4, 5, 6, 7, 8, 9), c(3, 3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5, 5.25, 5.5, 5.75, 6), FALSE, FALSE)"));  
.Internal(findInterval(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

