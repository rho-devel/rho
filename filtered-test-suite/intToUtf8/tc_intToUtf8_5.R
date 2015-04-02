expected <- NA_character_    
test(id=1, code={    
argv <- structure(list(x = NA_integer_), .Names = "x")    
do.call('intToUtf8', argv);    
},  o = expected);    
    
