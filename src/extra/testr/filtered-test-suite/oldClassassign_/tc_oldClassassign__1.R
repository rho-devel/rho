expected <- 1:10     
test(id=0, code={     
argv <- structure(list(structure(1:10, class = "foo"), value = character(0)), .Names = c("",      
"value"))     
do.call('oldClass<-', argv);     
},  o = expected);     
     
