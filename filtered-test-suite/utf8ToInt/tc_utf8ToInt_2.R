expected <- NA_integer_    
test(id=0, code={    
argv <- structure(list(x = NA_character_), .Names = "x")    
do.call('utf8ToInt', argv);    
},  o = expected);    
    
