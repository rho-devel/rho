expected <- eval(parse(text="structure(c(\"ANY\", \"ANY\", \"ANY\", \"ANY\", \"ANY\", \"ANY\"), .Dim = 2:3)"));             
test(id=0, code={             
argv <- eval(parse(text="list(\"ANY\", 2L, 3L, FALSE, NULL, FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

