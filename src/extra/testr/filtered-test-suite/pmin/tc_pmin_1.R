expected <- eval(parse(text="c(0, 0.25, 0.5, 0.75, 1)"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, c(0, 0.25, 0.5, 0.75, 1), 1)"));  
.Internal(`pmin`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

