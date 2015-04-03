expected <- eval(parse(text="22L"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 1L, 2L, 3L, 4L, 5L, 3L, 4L, 5L, 6L, 7L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), FALSE, TRUE)"));             
.Internal(anyDuplicated(argv[[1]], argv[[2]], argv[[3]]));             
}, o=expected);             

