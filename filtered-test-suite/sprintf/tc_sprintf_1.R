expected <- eval(parse(text="\"identical(fxy, c(1, 2, 3)) is not TRUE\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"%s is not TRUE\", \"identical(fxy, c(1, 2, 3))\")"));  
.Internal(`sprintf`(argv[[1]], argv[[2]]));  
}, o=expected);  

