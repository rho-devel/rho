expected <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 7, 7)
test(id=54, code={
argv <- list(c(1, 2, 3, 4, 7), c(3, 4, 5, 4, 2))
do.call('rep', argv);
},  o = expected);

