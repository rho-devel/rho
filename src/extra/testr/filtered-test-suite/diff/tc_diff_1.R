expected <- 0.0928175064153038
test(id=10, code={
argv <- structure(list(x = c(0.467590032349108, 0.560407538764412)), .Names = "x")
do.call('diff', argv);
},  o = expected);

