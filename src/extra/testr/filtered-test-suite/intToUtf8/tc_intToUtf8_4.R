expected <- NA_character_     
test(id=0, code={     
argv <- structure(list(x = NA_integer_, multiple = TRUE), .Names = c("x",      
"multiple"))     
do.call('intToUtf8', argv);     
},  o = expected);     
     
