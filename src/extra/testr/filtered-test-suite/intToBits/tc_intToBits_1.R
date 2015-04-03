expected <- eval(parse(text="raw(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(list())"));  
.Internal(intToBits(argv[[1]]));  
}, o=expected);  

