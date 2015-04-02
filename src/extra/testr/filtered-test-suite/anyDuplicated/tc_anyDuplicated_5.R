expected <- eval(parse(text="0L"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(NA, 9L, 13L), FALSE, FALSE)"));             
.Internal(anyDuplicated(argv[[1]], argv[[2]], argv[[3]]));             
}, o=expected);             

