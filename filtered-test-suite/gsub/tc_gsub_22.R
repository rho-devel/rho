expected <- "xbxcx"     
test(id=4, code={     
argv <- structure(list(pattern = "a*", replacement = "x", x = "baaaac",      
    perl = TRUE), .Names = c("pattern", "replacement", "x", "perl"     
))     
do.call('gsub', argv);     
},  o = expected);     
     
