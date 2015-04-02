expected <- "xbxcx"       
test(id=2, code={       
argv <- structure(list(pattern = "a*", replacement = "x", x = "baaaac"), .Names = c("pattern",        
"replacement", "x"))       
do.call('gsub', argv);       
},  o = expected);       
       
