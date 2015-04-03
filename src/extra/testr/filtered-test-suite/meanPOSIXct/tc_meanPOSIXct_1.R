expected <- structure(1412795929.08562, class = c("POSIXct", "POSIXt"))    
test(id=0, code={    
argv <- structure(list(x = structure(1412795929.08562, class = c("POSIXct",     
"POSIXt"))), .Names = "x")    
do.call('mean.POSIXct', argv);    
},  o = expected);    
    
