expected <- Inf     
test(id=1, code={     
argv <- list(Inf)     
do.call('lgamma', argv);     
}, w = "value out of range in 'lgamma'", o = expected);     
     
