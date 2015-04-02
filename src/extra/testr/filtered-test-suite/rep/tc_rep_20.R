expected <- c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L)
test(id=66, code={
argv <- structure(list(1:5, each = 2), .Names = c("", "each"))
do.call('rep', argv);
},  o = expected);

