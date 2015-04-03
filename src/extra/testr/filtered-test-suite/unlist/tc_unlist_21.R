expected <- eval(parse(text="structure(c(5900.92307692308, 6784.76923076923), .Names = c(\"1\", \"2\"))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(`1` = 5900.92307692308, `2` = 6784.76923076923), .Names = c(\"1\", \"2\")), FALSE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

