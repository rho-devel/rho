expected <- eval(parse(text="structure(list(xi = integer(0), yi = integer(0), x.alone = 1:5, y.alone = NULL), .Names = c(\"xi\", \"yi\", \"x.alone\", \"y.alone\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(0L, 0L, 0L, 0L, 0L), 0L, TRUE, FALSE)"));  
.Internal(`merge`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

