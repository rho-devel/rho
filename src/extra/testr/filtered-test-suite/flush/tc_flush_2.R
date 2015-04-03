expected <- eval(parse(text="NULL"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(2L, class = c(\"terminal\", \"connection\")))"));    
.Internal(flush(argv[[1]]));    
}, o=expected);    

