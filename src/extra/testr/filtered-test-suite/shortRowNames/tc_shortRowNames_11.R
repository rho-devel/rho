expected <- eval(parse(text="c(NA, -3L)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(age = c(40, 60, 80)), .Names = \"age\", row.names = c(NA, -3L), class = \"data.frame\"), 0L)"));                 
.Internal(shortRowNames(argv[[1]], argv[[2]]));                 
}, o=expected);                 

