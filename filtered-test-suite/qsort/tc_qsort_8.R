expected <- eval(parse(text="c(1, 42, 43, 83, 84, 124, 125, 166)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1, 42, 83, 124, 166, 43, 84, 125), FALSE)"));  
.Internal(`qsort`(argv[[1]], argv[[2]]));  
}, o=expected);  

