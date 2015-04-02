expected <- eval(parse(text="c(1L, 1L, 1L, 1L, 1L, 1L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(1:6, 6L)"));  
.Internal(`tabulate`(argv[[1]], argv[[2]]));  
}, o=expected);  

