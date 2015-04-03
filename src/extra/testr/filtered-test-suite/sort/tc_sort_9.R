expected <- eval(parse(text="c(\"M.user\", \"Soft\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"M.user\", \"Soft\"), FALSE)"));  
.Internal(`sort`(argv[[1]], argv[[2]]));  
}, o=expected);  

