expected <- structure(31, units = "days", class = "difftime")              
test(id=402, code={              
argv <- structure(list(x = structure(c(31, NA, NA, 31), units = "days", class = "difftime"),               
    na.rm = TRUE), .Names = c("x", "na.rm"))              
do.call('mean', argv);              
},  o = expected);              
              
