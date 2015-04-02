expected <- eval(parse(text="structure(list(xi = 4:5, yi = 4:5, x.alone = NULL, y.alone = NULL), .Names = c(\"xi\", \"yi\", \"x.alone\", \"y.alone\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(0L, 0L, 0L, 3L, 4L), c(0L, 0L, 0L, 3L, 4L), FALSE, FALSE)"));  
.Internal(merge(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

