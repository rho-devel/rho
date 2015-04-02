expected <- eval(parse(text="NULL"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(age = 1:65), .Names = \"age\"), 0L)"));                 
.Internal(shortRowNames(argv[[1]], argv[[2]]));                 
}, o=expected);                 

