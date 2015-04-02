expected <- 0.75             
test(id=726, code={             
argv <- structure(list(x = c(TRUE, FALSE, TRUE, TRUE)), .Names = "x")             
do.call('mean', argv);             
},  o = expected);             
             
