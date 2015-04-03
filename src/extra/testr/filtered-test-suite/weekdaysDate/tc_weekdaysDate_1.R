expected <- "Thursday"    
test(id=0, code={    
argv <- structure(list(x = structure(16352, class = "Date")), .Names = "x")    
do.call('weekdays.Date', argv);    
},  o = expected);    
    
