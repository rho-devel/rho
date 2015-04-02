expected <- NA_real_
test(id=1, code={
argv <- list(2, 3, NA)
do.call('max', argv);
},  o = expected);

