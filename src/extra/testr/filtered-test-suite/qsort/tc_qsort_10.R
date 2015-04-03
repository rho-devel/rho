expected <- eval(parse(text="c(1, 13, 14, 26, 38, 39, 51)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1, 13, 26, 38, 51, 14, 39), FALSE)"));  
.Internal(`qsort`(argv[[1]], argv[[2]]));  
}, o=expected);  

