expected <- eval(parse(text="c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, 0, 1, 2, 3, 4, 5, Inf, Inf, Inf, Inf, Inf)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, 0, 1, 2, 3, 4, 5, Inf, Inf, Inf, Inf, Inf), FALSE)"));  
.Internal(`qsort`(argv[[1]], argv[[2]]));  
}, o=expected);  

