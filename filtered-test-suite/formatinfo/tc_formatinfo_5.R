expected <- c(3L, 0L, 0L, 3L, 0L, 0L)     
test(id=8, code={     
argv <- structure(list(x = complex(real=Inf, imaginary=Inf)), .Names = "x")     
do.call('format.info', argv);     
},  o = expected);     
     
