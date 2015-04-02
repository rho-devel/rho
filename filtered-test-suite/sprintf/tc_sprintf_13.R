expected <- "1.235e+04"            
test(id=11, code={            
argv <- structure(list(fmt = "%9.4g", 12345.6789), .Names = c("fmt",             
""))            
do.call('sprintf', argv);            
},  o = expected);            
            
