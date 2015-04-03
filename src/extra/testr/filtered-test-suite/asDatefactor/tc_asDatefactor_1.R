expected <- structure(10958, class = "Date")    
test(id=0, code={    
argv <- structure(list(x = structure(1L, .Label = "2000-01-02", class = "factor")), .Names = "x")    
do.call('as.Date.factor', argv);    
},  o = expected);    
    
