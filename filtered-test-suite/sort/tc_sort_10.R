expected <- eval(parse(text="c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 10, 10.5, 20, 20.5)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(0, 1, 2, 3, 4, 5, 10, 20, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 10.5, 20.5), FALSE)"));  
.Internal(`sort`(argv[[1]], argv[[2]]));  
}, o=expected);  

