expected <- eval(parse(text="structure(integer(0), .Dim = c(1L, 0L))"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(1L, 0L))"));       
.Internal(col(argv[[1]]));       
}, o=expected);       

