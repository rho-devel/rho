expected <- eval(parse(text="c(1, 2, 3)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1, 2, 3), 3L, \"average\")"));  
.Internal(`rank`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

