expected <- eval(parse(text="integer(0)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(inner = integer(0), outer = integer(0)), .Names = c(\"inner\", \"outer\")), TRUE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

