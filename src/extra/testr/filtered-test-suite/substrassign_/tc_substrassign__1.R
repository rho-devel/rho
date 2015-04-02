expected <- c("na", NA, "naNANA")      
test(id=7, code={      
argv <- structure(list(x = c("NA", NA, "BANANA"), start = 1, stop = 2,       
    value = "na"), .Names = c("x", "start", "stop", "value"))      
do.call('substr<-', argv);      
},  o = expected);      
      
