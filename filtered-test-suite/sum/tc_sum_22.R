expected <- NA_real_
test(id=915, code={
argv <- list(2, 3, NA)
do.call('sum', argv);
},  o = expected);

