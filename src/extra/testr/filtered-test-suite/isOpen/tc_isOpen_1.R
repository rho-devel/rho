expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(2L, class = c(\"terminal\", \"connection\")), 0L)"));  
.Internal(`isOpen`(argv[[1]], argv[[2]]));  
}, o=expected);  

