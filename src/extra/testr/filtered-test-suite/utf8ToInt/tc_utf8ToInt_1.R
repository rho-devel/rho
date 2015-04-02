expected <- eval(parse(text="c(108L, 97L, 115L, 121L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"lasy\")"));  
.Internal(utf8ToInt(argv[[1]]));  
}, o=expected);  

