expected <- structure(14580, class = "Date")    
test(id=0, code={    
argv <- structure(list(x = structure(c(14579, 14580), class = "Date"),     
    2), .Names = c("x", ""))    
do.call('[.Date', argv);    
},  o = expected);    
    
