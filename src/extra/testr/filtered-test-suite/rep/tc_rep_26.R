expected <- c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 
5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 
9L, 9L, 9L, 9L, 10L, 10L, 10L, 10L, 11L, 11L, 11L, 11L, 12L, 
12L, 12L, 12L, 13L, 13L, 13L, 13L, 14L, 14L, 14L, 14L)
test(id=64, code={
argv <- list(1:14, c(3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4))
do.call('rep', argv);
},  o = expected);

