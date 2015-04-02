expected <- eval(parse(text="TRUE"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(sec = NA_real_, min = NA_integer_, hour = NA_integer_, mday = NA_integer_, mon = NA_integer_, year = NA_integer_, wday = NA_integer_, yday = NA_integer_, isdst = -1L), .Names = c(\"sec\", \"min\", \"hour\", \"mday\", \"mon\", \"year\", \"wday\", \"yday\", \"isdst\"), class = c(\"POSIXlt\", \"POSIXt\"), tzone = \"GMT\"))"));        
do.call(`is.na`, argv);        
}, o=expected);        

