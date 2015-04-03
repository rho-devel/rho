expected <- structure(c(4.6, 5.1, 5.15, 5.236, 5.4, 5.8), .Names = c("Min.", 
"1st Qu.", "Median", "Mean", "3rd Qu.", "Max."), class = c("summaryDefault", 
"table"))
test(id=24, code={
argv <- structure(list(object = c(5.1, 5, 5.4, 5.4, 5.8, 5.7, 5.4, 5.1, 
5.7, 5.1, 5.1, 4.6, 5.2, 5.2, 5.5, 5.5, 4.9, 5, 5, 5.1, 5.1, 
5.3)), .Names = "object")
do.call('summary', argv);
},  o = expected);

