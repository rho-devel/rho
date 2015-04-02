expected <- eval(parse(text="c(10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L)"));            
test(id=0, code={            
argv <- eval(parse(text="list(1:10, TRUE)"));            
.Internal(sort(argv[[1]], argv[[2]]));            
}, o=expected);            

