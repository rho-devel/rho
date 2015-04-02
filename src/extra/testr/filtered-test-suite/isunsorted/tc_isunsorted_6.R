expected <- FALSE      
test(id=4, code={      
argv <- structure(list(x = structure(list(x = c(2L, 1L)), .Names = "x", row.names = c(NA,       
-2L), class = "data.frame")), .Names = "x")      
do.call('is.unsorted', argv);      
},  o = expected);      
      
