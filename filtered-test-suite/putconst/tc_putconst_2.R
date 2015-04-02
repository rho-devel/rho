expected <- eval(parse(text="0L"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(list(), NULL), 1, list())"));  
.Internal(putconst(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

