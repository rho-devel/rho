expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(2L, class = c(\"terminal\", \"connection\")), FALSE, TRUE, FALSE)"));  
.Internal(`sink`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

