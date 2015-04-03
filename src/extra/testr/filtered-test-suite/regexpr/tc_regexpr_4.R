expected <- structure(NA_integer_, match.length = NA_integer_)    
test(id=5, code={    
argv <- structure(list(pattern = "[a-z]", text = NA), .Names = c("pattern",     
"text"))    
do.call('regexpr', argv);    
},  o = expected);    
    
