expected <- eval(parse(text="character(0)"));        
test(id=0, code={        
argv <- eval(parse(text="list(character(0), TRUE)"));        
.Internal(make.names(argv[[1]], argv[[2]]));        
}, o=expected);        

