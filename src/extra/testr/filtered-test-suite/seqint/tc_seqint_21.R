test(id=3, code={      
argv <- list(NaN)      
do.call('seq.int', argv);      
}, e = "'from' cannot be NA, NaN or infinite");      
      
