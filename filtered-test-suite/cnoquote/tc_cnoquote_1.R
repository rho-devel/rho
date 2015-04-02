expected <- structure(c(".", ".", "|", ".", ".", "|", ".", "."), class = "noquote")    
test(id=0, code={    
argv <- structure(list(structure(c(".", ".", "|", ".", ".", "|", ".",     
"."), .Dim = c(2L, 4L), .Dimnames = list(NULL, c("", "", "",     
"")), class = "noquote")), .Names = "")    
do.call('c.noquote', argv);    
},  o = expected);    
    
