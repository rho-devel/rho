expected <- NA_real_              
test(id=954, code={              
argv <- structure(list(x = structure(c(2L, 1L, 2L, 2L), .Label = c("FALSE",               
"TRUE"), class = "factor")), .Names = "x")              
do.call('mean.default', argv);              
},  o = expected);              
              
