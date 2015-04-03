expected <- NA_real_
test(id=2, code={
argv <- list(2, 3, NA)
do.call('min', argv);
},  o = expected);

