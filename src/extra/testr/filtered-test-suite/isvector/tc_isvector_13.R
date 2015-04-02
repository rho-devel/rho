expected <- eval(parse(text="TRUE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(list(\"‘FUN’ is found by a call to ‘match.fun’ and typically   is specified as a function or a symbol (e.g. a backquoted name) or a   character string specifying a function to be searched for from the   environment of the call to ‘lapply’.\"), \"any\")"));                  
.Internal(is.vector(argv[[1]], argv[[2]]));                  
}, o=expected);                  

