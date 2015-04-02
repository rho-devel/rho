expected <- structure(-3621, class = "Date")     
test(id=0, code={     
argv <- structure(list(x = structure(-3620.8, class = "Date")), .Names = "x")     
do.call('trunc.Date', argv);     
},  o = expected);     
     
