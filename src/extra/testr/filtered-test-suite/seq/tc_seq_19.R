expected <- 1:4      
test(id=0, code={      
argv <- list(structure(list(num = 1:4, fac = structure(11:14, .Label = c("a",       
"b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",       
"o"), class = "factor"), date = structure(c(15065, 15066, 15067,       
15068), class = "Date"), pv = structure(list(1:3, 4:5, 6:7, 8:10), class = c("package_version",       
"numeric_version"))), .Names = c("num", "fac", "date", "pv"), row.names = c(NA,       
-4L), class = "data.frame"))      
do.call('seq_along', argv);      
},  o = expected);      
      
