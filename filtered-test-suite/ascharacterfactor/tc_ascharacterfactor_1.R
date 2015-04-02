expected <- c("AB", "CD", NA)     
test(id=2, code={     
argv <- structure(list(x = structure(c(1L, 2L, NA), .Label = c("AB",      
"CD"), class = "factor")), .Names = "x")     
do.call('as.character.factor', argv);     
},  o = expected);     
     
