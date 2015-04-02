expected <- "UserHook::stats4::onLoad"    
test(id=1, code={    
argv <- structure(list(pkgname = "stats4", event = "onLoad"), .Names = c("pkgname",     
"event"))    
do.call('packageEvent', argv);    
},  o = expected);    
    
