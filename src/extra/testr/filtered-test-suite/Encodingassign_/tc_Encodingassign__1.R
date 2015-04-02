expected <- "abc"    
test(id=0, code={    
argv <- structure(list(x = "abc", value = "UTF-8"), .Names = c("x", "value"    
))    
do.call('Encoding<-', argv);    
},  o = expected);    
    
