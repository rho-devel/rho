expected <- eval(parse(text="structure(FALSE, .Dim = c(1L, 1L))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(FALSE, .Dim = c(1L, 1L)), NULL, NULL, TRUE, FALSE, NULL)"));     
.Internal(prmatrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));     
}, o=expected);     

