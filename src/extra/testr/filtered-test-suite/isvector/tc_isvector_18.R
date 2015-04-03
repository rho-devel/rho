expected <- eval(parse(text="FALSE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(age = 60), .Names = \"age\", row.names = c(NA, -1L), class = \"data.frame\"), \"numeric\")"));                  
.Internal(is.vector(argv[[1]], argv[[2]]));                  
}, o=expected);                  

