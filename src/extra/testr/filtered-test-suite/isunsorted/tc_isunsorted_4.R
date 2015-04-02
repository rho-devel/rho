expected <- FALSE      
test(id=3, code={      
argv <- structure(list(x = c(1L, 2L, 3L, 5L, 5L, 6L, 6L, 7L, 7L, 7L,       
7L, 7L, 8L, 8L, 9L, 9L, 10L, 12L, 12L, 12L, 12L, 13L, 15L, 20L,       
28L)), .Names = "x")      
do.call('is.unsorted', argv);      
},  o = expected);      
      
