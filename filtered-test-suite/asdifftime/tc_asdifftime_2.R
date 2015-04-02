expected <- structure(c(3.33333333333333, 23.25, NA), units = "hours", class = "difftime")      
test(id=2, code={      
argv <- structure(list(tim = c("3:20", "23:15", "2:"), format = "%H:%M"), .Names = c("tim",       
"format"))      
do.call('as.difftime', argv);      
},  o = expected);      
      
