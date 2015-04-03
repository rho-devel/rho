expected <- NA          
test(id=13, code={          
argv <- list("NA")          
do.call('any', argv);          
}, w = "coercing argument of type 'character' to logical", o = expected);          
          
