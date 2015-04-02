expected <- eval(parse(text="\"text\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"text\", 6, TRUE)"));  
.Internal(`abbreviate`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

