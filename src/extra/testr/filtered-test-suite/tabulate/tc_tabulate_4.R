expected <- eval(parse(text="c(2L, 0L, 3L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 3L, 1L, 0L, 0L, 2L, 2L)"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(1L, 9L, 13L, 25L, 11L, 24L, 3L, 20L, 20L, 15L, 20L, 14L, 24L, 19L, 12L, 8L, 1L, 11L, 4L, 3L, 21L, 25L, 10L, 3L, 12L), 25L)"));             
.Internal(tabulate(argv[[1]], argv[[2]]));             
}, o=expected);             

