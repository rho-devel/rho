expected <- eval(parse(text="structure(2.10239639473973e-05, .Dim = c(1L, 1L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(2.10239639473973e-05, c(1L, 1L), NULL)"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

