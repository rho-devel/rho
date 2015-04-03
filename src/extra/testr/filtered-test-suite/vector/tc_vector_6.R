expected <- c(0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i, 0+0i)     
test(id=0, code={     
argv <- structure(list(mode = "complex", length = 7), .Names = c("mode",      
"length"))     
do.call('vector', argv);     
},  o = expected);     
     
