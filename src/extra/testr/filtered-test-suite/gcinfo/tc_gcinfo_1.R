expected <- eval(parse(text="FALSE"));  
test(id=0, code={  
argv <- eval(parse(text="list(list())"));  
.Internal(gcinfo(argv[[1]]));  
}, o=expected);  

