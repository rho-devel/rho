expected <- eval(parse(text="TRUE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(list(), \"list\")"));                  
.Internal(is.vector(argv[[1]], argv[[2]]));                  
}, o=expected);                  

