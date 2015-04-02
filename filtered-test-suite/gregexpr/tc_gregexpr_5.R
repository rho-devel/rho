expected <- list(structure(1:3, match.length = c(0L, 0L, 0L), useBytes = TRUE))    
test(id=0, code={    
argv <- structure(list(pattern = "", text = "abc", fixed = TRUE), .Names = c("pattern",     
"text", "fixed"))    
do.call('gregexpr', argv);    
},  o = expected);    
    
