expected <- eval(parse(text="structure(c(\"315.45\", \"363.01\", \"405.02\", \"443.06\", \"478.09\", \"510.72\"), .Dim = c(1L, 6L), .Dimnames = list(\"1979\", c(\"Jan\", \"Feb\", \"Mar\", \"Apr\", \"May\", \"Jun\")))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(\"315.45\", \"363.01\", \"405.02\", \"443.06\", \"478.09\", \"510.72\"), 1L, 1, TRUE, list(\"1979\", c(\"Jan\", \"Feb\", \"Mar\", \"Apr\", \"May\", \"Jun\")), FALSE, TRUE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

