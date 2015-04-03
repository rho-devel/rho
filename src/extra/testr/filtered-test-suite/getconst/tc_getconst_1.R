expected <- eval(parse(text="list(list())"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(list(), NULL), 1)"));  
.Internal(getconst(argv[[1]], argv[[2]]));  
}, o=expected);  

