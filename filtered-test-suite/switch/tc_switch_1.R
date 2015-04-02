expected <- "posS"     
test(id=0, code={     
argv <- structure(list("forward", forward = "posS", reverse = "negS"), .Names = c("",      
"forward", "reverse"))     
do.call('switch', argv);     
},  o = expected);     
     
