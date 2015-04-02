expected <- c(0, 0)
test(id=12, code={
argv <- structure(list(x = c(0, 0)), .Names = "x")
do.call('sample', argv);
},  o = expected);

