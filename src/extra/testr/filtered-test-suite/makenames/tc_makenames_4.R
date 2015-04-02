expected <- eval(parse(text="\"..adfl.row.names\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"..adfl.row.names\", TRUE)"));  
.Internal(`make.names`(argv[[1]], argv[[2]]));  
}, o=expected);  

