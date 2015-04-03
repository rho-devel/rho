expected <- eval(parse(text="\"X.2a\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\".2a\", TRUE)"));  
.Internal(`make.names`(argv[[1]], argv[[2]]));  
}, o=expected);  

