expected <- eval(parse(text="c(1, 173, 174, 346, 518, 519, 691)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1, 173, 346, 518, 691, 174, 519), FALSE)"));  
.Internal(`qsort`(argv[[1]], argv[[2]]));  
}, o=expected);  

