expected <- -1022     
test(id=1, code={     
argv <- list(2.2250738585072e-308)     
do.call('log2', argv);     
},  o = expected);     
     
