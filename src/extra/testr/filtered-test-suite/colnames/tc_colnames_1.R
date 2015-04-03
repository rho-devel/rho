expected <- c("", "B", "C", "D")       
test(id=14, code={       
argv <- structure(list(x = structure(c(1.00000000000001, 2, 3, 4, 5,        
6, 7, 8, 9, 10, 0.999999999999998, 4, 9, 16, 25, 36, 49, 64,        
81, 100, 5.39416105805496e-14, 2, 6, 12, 20, 30, 42, 56, 72,        
90, 1, 0.999999999999999, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(10L,        
4L), .Dimnames = list(NULL, c("", "B", "C", "D")))), .Names = "x")       
do.call('colnames', argv);       
},  o = expected);       
       
