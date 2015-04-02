expected <- eval(parse(text="structure(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(FALSE, TRUE, TRUE, TRUE), 4L, 4L)"));           
.Internal(diag(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

