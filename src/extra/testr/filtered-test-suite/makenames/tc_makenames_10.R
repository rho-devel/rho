expected <- eval(parse(text="c(\"X\", \"X\", \"bady\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"\", \"\", \"bady\"), TRUE)"));        
.Internal(make.names(argv[[1]], argv[[2]]));        
}, o=expected);        

