expected <- 3
test(id=23, code={
argv <- structure(list(2, 3, NA, na.rm = TRUE), .Names = c("", "", "", 
"na.rm"))
do.call('max', argv);
},  o = expected);

