expected <- eval(parse(text="\"X\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"\", TRUE)"));  
.Internal(`make.names`(argv[[1]], argv[[2]]));  
}, o=expected);  

