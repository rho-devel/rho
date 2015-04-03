expected <- eval(parse(text="structure(8.91763605923317e+38, .Names = \"1\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(`1` = 8.91763605923317e+38), .Names = \"1\"), FALSE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

