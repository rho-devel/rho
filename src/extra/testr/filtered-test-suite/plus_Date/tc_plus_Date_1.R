expected <- structure(14580, class = "Date")      
test(id=2, code={      
argv <- structure(list(e1 = structure(1, units = "days", class = "difftime"),       
    e2 = structure(14579, class = "Date")), .Names = c("e1",       
"e2"))      
do.call('+.Date', argv);      
},  o = expected);      
      
