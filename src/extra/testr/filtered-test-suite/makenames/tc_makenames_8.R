expected <- eval(parse(text="\"NA.\""));  
test(id=0, code={  
argv <- eval(parse(text="list(NA_character_, TRUE)"));  
.Internal(`make.names`(argv[[1]], argv[[2]]));  
}, o=expected);  

