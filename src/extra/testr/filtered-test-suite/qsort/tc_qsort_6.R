expected <- eval(parse(text="c(1, 2, 3, 4, 5, 6, 7, 8)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1, 2, 4, 6, 8, 3, 5, 7), FALSE)"));  
.Internal(`qsort`(argv[[1]], argv[[2]]));  
}, o=expected);  

