expected <- eval(parse(text="structure(NA, .Dim = 1L, .Dimnames = list(\"1\"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(NA, 1L, list(\"1\"))"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

