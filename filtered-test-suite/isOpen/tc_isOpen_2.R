expected <- eval(parse(text="FALSE"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, 2L)"));  
.Internal(isOpen(argv[[1]], argv[[2]]));  
}, o=expected);  

