expected <- eval(parse(text="structure(c(1L, 0L), .Dim = 2L, .Dimnames = structure(list(object = c(\"FALSE\", NA)), .Names = \"object\"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(1L, 0L), 2L, structure(list(object = c(\"FALSE\", NA)), .Names = \"object\"))"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

