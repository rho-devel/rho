expected <- FALSE     
test(id=2, code={     
argv <- structure(list(x = c("A", "B", "C", "D", "E", "F", "G", "H",      
"I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U",      
"V", "W", "X", "Y", "Z")), .Names = "x")     
do.call('is.unsorted', argv);     
},  o = expected);     
     
