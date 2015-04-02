expected <- eval(parse(text="c(1L, 1L, 1L, 1L, 1L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(-1, -1, -1, -1, -1), c(-1.001, -1, -0.999), TRUE, FALSE)"));  
.Internal(`bincode`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

