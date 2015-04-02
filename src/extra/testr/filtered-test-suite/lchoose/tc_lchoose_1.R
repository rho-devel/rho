expected <- eval(parse(text="0"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, FALSE)"));  
.Internal(lchoose(argv[[1]], argv[[2]]));  
}, o=expected);  

