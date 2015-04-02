expected <- eval(parse(text="\"/home/roman/r-instrumented/library/translations\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"splines\", \"/home/roman/r-instrumented/library/translations\")"));  
.Internal(`bindtextdomain`(argv[[1]], argv[[2]]));  
}, o=expected);  

