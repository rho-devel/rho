expected <- FALSE
test(id=0, code={
argv <- list(2L, TRUE, FALSE, FALSE)
do.call('switch', argv);
},  o = expected);

