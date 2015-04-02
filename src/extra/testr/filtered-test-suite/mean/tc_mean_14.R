expected <- NA_real_             
test(id=677, code={             
argv <- structure(list(x = structure(c(2L, 1L, 2L, 2L), .Label = c("FALSE",              
"TRUE"), class = "factor")), .Names = "x")             
do.call('mean', argv);             
}, w = "argument is not numeric or logical: returning NA", o = expected);             
             
