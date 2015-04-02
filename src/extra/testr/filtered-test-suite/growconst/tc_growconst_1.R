expected <- eval(parse(text="list(list(), NULL)"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(list()))"));  
.Internal(growconst(argv[[1]]));  
}, o=expected);  

