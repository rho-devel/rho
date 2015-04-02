expected <- "string"    
test(id=2, code={    
argv <- structure(list(x = as.raw(c(0x73, 0x74, 0x72, 0x69, 0x6e, 0x67    
))), .Names = "x")    
do.call('rawToChar', argv);    
},  o = expected);    
    
