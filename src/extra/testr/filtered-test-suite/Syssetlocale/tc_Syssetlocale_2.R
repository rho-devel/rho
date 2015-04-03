expected <- "C"    
test(id=0, code={    
argv <- structure(list(category = "LC_TIME", locale = "C"), .Names = c("category",     
"locale"))    
do.call('Sys.setlocale', argv);    
},  o = expected);    
    
