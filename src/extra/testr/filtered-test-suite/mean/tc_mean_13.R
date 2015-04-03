expected <- 5.5          
test(id=189, code={          
argv <- structure(list(x = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 50), trim = 0.5), .Names = c("x",           
"trim"))          
do.call('mean', argv);          
},  o = expected);          
          
