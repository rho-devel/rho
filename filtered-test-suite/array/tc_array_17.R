expected <- eval(parse(text="structure(c(\"\", \"\", \"\"), .Dim = c(3L, 1L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(\"\", \"\", \"\"), c(3, 1), NULL)"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

