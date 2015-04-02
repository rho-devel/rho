expected <- c("N", NA, "B")      
test(id=5, code={      
argv <- structure(list(text = c("NA", NA, "BANANA"), first = 1, last = 1), .Names = c("text",       
"first", "last"))      
do.call('substring', argv);      
},  o = expected);      
      
