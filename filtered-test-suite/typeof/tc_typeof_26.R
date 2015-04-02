expected <- "complex"       
test(id=2, code={       
argv <- structure(list(x = c(1.1+0i, NA, 3+0i)), .Names = "x")       
do.call('typeof', argv);       
},  o = expected);       
       
