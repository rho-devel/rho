expected <- list(structure(1:3, match.length = c(0L, 0L, 0L), useBytes = TRUE))    
test(id=2, code={    
argv <- structure(list(pattern = "", text = "abc", perl = TRUE), .Names = c("pattern",     
"text", "perl"))    
do.call('gregexpr', argv);    
},  o = expected);    
    
