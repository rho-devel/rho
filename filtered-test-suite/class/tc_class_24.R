expected <- "numeric"       
test(id=7, code={       
argv <- list(c(71.128, 69.70625, 70.9566666666667, 71.7, 71.435, 72.5766666666667,        
70.6916666666667))       
do.call('class', argv);       
},  o = expected);       
       
