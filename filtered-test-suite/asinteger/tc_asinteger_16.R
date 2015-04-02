expected <- eval(parse(text="c(100L, 0L, NA, NA, NA, 3L, NA)"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(100, -1e-13, Inf, -Inf, NaN, 3.14159265358979, NA), .Names = c(\" 100\", \"-1e-13\", \" Inf\", \"-Inf\", \" NaN\", \"3.14\", \"  NA\")))"));    
do.call(`as.integer`, argv);    
}, o=expected);    

