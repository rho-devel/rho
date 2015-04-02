expected <- "Mean relative difference: 24"
test(id=1, code={
argv <- structure(list(target = 0.261799387799149, current = 6.54498469497874), .Names = c("target", 
"current"))
do.call('all.equal', argv);
},  o = expected);

