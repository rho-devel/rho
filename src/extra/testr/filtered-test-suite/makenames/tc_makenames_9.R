expected <- eval(parse(text="c(\"Subject\", \"predict.fixed\", \"predict.Subject\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"Subject\", \"predict.fixed\", \"predict.Subject\"), TRUE)"));        
.Internal(make.names(argv[[1]], argv[[2]]));        
}, o=expected);        

