test(id=0, code={      
argv <- list(NaN, NaN)      
do.call('seq.int', argv);      
}, e = "'from' cannot be NA, NaN or infinite");      
      
