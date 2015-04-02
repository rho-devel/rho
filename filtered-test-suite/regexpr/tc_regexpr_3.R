expected <- structure(c(1L, -1L, 1L), match.length = c(1L, -1L, 1L), useBytes = TRUE)      
test(id=4, code={      
argv <- structure(list(pattern = "\\d", text = c("1", "B", "3")), .Names = c("pattern",       
"text"))      
do.call('regexpr', argv);      
},  o = expected);      
      
