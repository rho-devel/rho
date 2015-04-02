expected <- FALSE      
test(id=17, code={      
argv <- list(complex(real=3, imaginary=-Inf))      
do.call('is.null', argv);      
},  o = expected);      
      
