expected <- TRUE      
test(id=3, code={      
argv <- list(c(1L, 0L, NA, 1L))      
do.call('is.integer', argv);      
},  o = expected);      
      
