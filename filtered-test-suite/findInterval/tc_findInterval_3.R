expected <- eval(parse(text="integer(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(numeric(0), numeric(0), FALSE, FALSE)"));  
.Internal(findInterval(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

