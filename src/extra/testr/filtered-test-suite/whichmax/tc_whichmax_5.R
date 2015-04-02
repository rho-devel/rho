expected <- eval(parse(text="integer(0)"));    
test(id=0, code={    
argv <- eval(parse(text="list(NULL)"));    
.Internal(which.max(argv[[1]]));    
}, o=expected);    

