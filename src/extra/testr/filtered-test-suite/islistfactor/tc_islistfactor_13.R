expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(`1` = 1.97626258336499e-323), .Names = \"1\"), FALSE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

