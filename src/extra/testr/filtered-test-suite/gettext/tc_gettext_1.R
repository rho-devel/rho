expected <- eval(parse(text="\"Loading required package: %s\""));  
test(id=0, code={  
argv <- eval(parse(text="list(NULL, \"Loading required package: %s\")"));  
.Internal(`gettext`(argv[[1]], argv[[2]]));  
}, o=expected);  

