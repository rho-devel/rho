expected <- c("1", "2")       
test(id=0, code={       
argv <- structure(list(x = structure(list(x = 3:4), .Names = "x", row.names = c(NA,        
-2L), class = "data.frame")), .Names = "x")       
do.call('rownames', argv);       
},  o = expected);       
       
