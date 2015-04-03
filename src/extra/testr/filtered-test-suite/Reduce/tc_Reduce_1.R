expected <- c(1L, 3L, 6L, 10L, 15L, 21L, 28L)      
test(id=5, code={      
argv <- structure(list(f = "+", x = 1:7, accumulate = TRUE), .Names = c("f",       
"x", "accumulate"))      
do.call('Reduce', argv);      
},  o = expected);      
      
