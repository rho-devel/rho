expected <- TRUE    
test(id=0, code={    
argv <- structure(list(pattern = "length", x = "Lengths: 0, 1", ignore.case = TRUE), .Names = c("pattern",     
"x", "ignore.case"))    
do.call('grepl', argv);    
},  o = expected);    
    
