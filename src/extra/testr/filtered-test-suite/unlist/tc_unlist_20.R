expected <- eval(parse(text="structure(2.47032822920623e-323, .Names = \"1\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(`1` = 2.47032822920623e-323), .Names = \"1\"), FALSE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

