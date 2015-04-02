expected <- eval(parse(text="\"head\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"head\", TRUE)"));  
.Internal(`make.names`(argv[[1]], argv[[2]]));  
}, o=expected);  

