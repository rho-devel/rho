expected <- eval(parse(text="\"FALSE.\""));        
test(id=0, code={        
argv <- eval(parse(text="list(\"FALSE\", TRUE)"));        
.Internal(make.names(argv[[1]], argv[[2]]));        
}, o=expected);        

