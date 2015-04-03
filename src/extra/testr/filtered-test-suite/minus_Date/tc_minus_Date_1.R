expected <- structure(14579, class = "Date")     
test(id=0, code={     
argv <- structure(list(e1 = structure(14580, class = "Date"), e2 = structure(1, units = "days", class = "difftime")), .Names = c("e1",      
"e2"))     
do.call('-.Date', argv);     
},  o = expected);     
     
