expected <- eval(parse(text="c(0.00508571428571428, 0.876285714285715)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(0.00508571428571428, 0.876285714285715), structure(1L, class = c(\"terminal\", \"connection\")), 69)"));  
.Internal(dput(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

