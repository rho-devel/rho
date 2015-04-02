expected <- eval(parse(text="structure(c(0.342020143325669, 0, -0.939692620785908, 0, 1, 0, 0.939692620785908, 0, 0.342020143325669), .Dim = c(3L, 3L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(0.342020143325669, 0, -0.939692620785908, 0, 1, 0, 0.939692620785908, 0, 0.342020143325669), 3, 3, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

