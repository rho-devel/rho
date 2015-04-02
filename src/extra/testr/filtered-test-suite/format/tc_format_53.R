expected <- "0.040"
test(id=17, code={
argv <- structure(list(x = 0.04, digits = 3, nsmall = 3), .Names = c("x", 
"digits", "nsmall"))
do.call('format', argv);
},  o = expected);

