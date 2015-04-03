expected <- eval(parse(text="NULL"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(NULL), TRUE, FALSE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

