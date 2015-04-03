expected <- 16351.8259046444     
test(id=0, code={     
argv <- structure(list(x = structure(16351.8259046444, units = "days", class = "difftime", origin = structure(0, class = c("POSIXct",      
"POSIXt"), tzone = "GMT"))), .Names = "x")     
do.call('as.double.difftime', argv);     
},  o = expected);     
     
