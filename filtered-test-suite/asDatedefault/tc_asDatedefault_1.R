expected <- structure(numeric(0), class = "Date")    
test(id=0, code={    
argv <- structure(list(x = logical(0)), .Names = "x")    
do.call('as.Date.default', argv);    
},  o = expected);    
    
