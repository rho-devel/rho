expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE)"));  
.Internal(clearPushBack(argv[[1]]));  
}, o=expected);  

