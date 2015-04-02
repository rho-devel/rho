expected <- c("a", "a", "a", "b", "b", "b", "c", "c", "c")
test(id=6, code={
argv <- structure(list(c("a", "b", "c"), each = 3), .Names = c("", "each"
))
do.call('rep', argv);
},  o = expected);

