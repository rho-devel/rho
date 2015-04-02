expected <- eval(parse(text="\"x1\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"x1\", FALSE)"));  
.Internal(`sort`(argv[[1]], argv[[2]]));  
}, o=expected);  

