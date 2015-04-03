expected <- NA_character_      
test(id=8, code={      
argv <- structure(list(x = "abcde", start = NA, stop = 3, value = "abc"), .Names = c("x",       
"start", "stop", "value"))      
do.call('substr<-', argv);      
},  o = expected);      
      
