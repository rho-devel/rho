expected <- eval(parse(text="3L"));  
test(id=0, code={  
argv <- eval(parse(text="list(3L, FALSE)"));  
.Internal(`qsort`(argv[[1]], argv[[2]]));  
}, o=expected);  

