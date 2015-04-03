expected <- eval(parse(text="structure(logical(0), .Dim = 0L)"));              
test(id=0, code={              
argv <- eval(parse(text="list(FALSE, FALSE, NULL)"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

