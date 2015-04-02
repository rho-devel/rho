expected <- c(3L, 0L, 0L, 3L, 0L, 0L)     
test(id=4, code={     
argv <- structure(list(x = c(complex(real=NaN, imaginary=NaN), NA)), .Names = "x")     
do.call('format.info', argv);     
},  o = expected);     
     
