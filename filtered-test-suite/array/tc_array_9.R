expected <- eval(parse(text="structure(integer(0), .Dim = c(1L, 0L), .Dimnames = structure(list(\"1\", NULL), .Names = c(\"\", \"\")))"));              
test(id=0, code={              
argv <- eval(parse(text="list(integer(0), c(1L, 0L), structure(list(\"1\", NULL), .Names = c(\"\", \"\")))"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

