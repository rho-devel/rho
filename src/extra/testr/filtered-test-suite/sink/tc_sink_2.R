expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(-1L, FALSE, FALSE, FALSE)"));  
.Internal(`sink`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

