expected <- structure("3.1.1", .Names = "version")    
test(id=0, code={    
argv <- structure(list(ns = "stats"), .Names = "ns")    
do.call('getNamespaceVersion', argv);    
},  o = expected);    
    
