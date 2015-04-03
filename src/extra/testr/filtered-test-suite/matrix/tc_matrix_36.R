expected <- eval(parse(text="structure(c(0, 0, 0, 0, 0, 0, 4.94065645841247e-324, 0, 0, 0, 0, 0), .Dim = c(12L, 1L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(0, 0, 0, 0, 0, 0, 4.94065645841247e-324, 0, 0, 0, 0, 0), structure(12L, .Names = \"1\"), 1L, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

