expected <- "numeric"      
test(id=8, code={      
argv <- structure(list(x = NA_real_), .Names = "x")      
do.call('mode', argv);      
},  o = expected);      
      
