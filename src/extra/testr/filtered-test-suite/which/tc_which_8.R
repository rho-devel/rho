expected <- eval(parse(text="c(3L, 6L, 9L, 12L)"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE), .Dim = 12L))"));            
.Internal(which(argv[[1]]));            
}, o=expected);            

