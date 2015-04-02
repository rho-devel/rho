expected <- eval(parse(text="logical(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(logical(0), NULL)"));  
.Internal('comment<-'(argv[[1]], argv[[2]]));  
}, o=expected);  

