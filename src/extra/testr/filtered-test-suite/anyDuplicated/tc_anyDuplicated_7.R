expected <- eval(parse(text="0L"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(-6, -3, 0, 3, 6, 9, 12, 15), FALSE, FALSE)"));             
.Internal(anyDuplicated(argv[[1]], argv[[2]], argv[[3]]));             
}, o=expected);             

