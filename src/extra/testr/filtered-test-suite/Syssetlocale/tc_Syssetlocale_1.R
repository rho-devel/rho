expected <- eval(parse(text="\"C\""));  
test(id=0, code={  
argv <- eval(parse(text="list(3L, \"C\")"));  
.Internal(`Sys.setlocale`(argv[[1]], argv[[2]]));  
}, o=expected);  

