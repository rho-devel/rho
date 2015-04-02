expected <- "integer"          
test(id=6, code={          
argv <- structure(list(x = c(NA_integer_, NA_integer_, NA_integer_)), .Names = "x")          
do.call('typeof', argv);          
},  o = expected);          
          
