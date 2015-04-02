expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(), .Names = character(0)), TRUE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

