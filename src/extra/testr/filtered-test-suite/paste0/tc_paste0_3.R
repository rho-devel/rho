expected <- eval(parse(text="\"\\\"1\\\",\\\"2\\\",NA\""));                
test(id=0, code={                
argv <- eval(parse(text="list(list(c(\"\\\"1\\\"\", \"\\\"2\\\"\", NA)), \",\")"));                
.Internal(paste0(argv[[1]], argv[[2]]));                
}, o=expected);                

