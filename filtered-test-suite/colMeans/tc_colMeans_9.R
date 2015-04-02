expected <- eval(parse(text="0"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(FALSE, .Dim = c(1L, 1L)), 1, 1, TRUE)"));         
.Internal(colMeans(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));         
}, o=expected);         

