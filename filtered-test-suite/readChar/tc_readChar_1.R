expected <- c("ABC", "DEF", "", "GHI", "J")    
test(id=2, code={    
argv <- structure(list(con = as.raw(c(0x41, 0x42, 0x43, 0x44, 0x45, 0x46,     
0x47, 0x48, 0x49, 0x4a)), nchars = c(3, 3, 0, 3, 3, 3)), .Names = c("con",     
"nchars"))    
do.call('readChar', argv);    
},  o = expected);    
    
