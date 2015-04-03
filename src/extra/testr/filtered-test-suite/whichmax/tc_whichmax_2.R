expected <- eval(parse(text="structure(1L, .Names = \"d\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(TRUE, FALSE), .Names = c(\"d\", \"I(as.numeric(d)^2)\")))"));    
.Internal(which.max(argv[[1]]));    
}, o=expected);    

