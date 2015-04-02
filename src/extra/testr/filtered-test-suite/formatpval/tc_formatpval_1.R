expected <- "0.201"
test(id=0, code={
argv <- structure(list(pv = 0.200965994008331, digits = 3), .Names = c("pv", 
"digits"))
do.call('format.pval', argv);
},  o = expected);

