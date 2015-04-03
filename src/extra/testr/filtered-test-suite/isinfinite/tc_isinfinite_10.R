expected <- eval(parse(text="structure(c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE), .Names = c(\" 100\", \"-1e-13\", \" Inf\", \"-Inf\", \" NaN\", \"3.14\", \"  NA\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(100, -1e-13, Inf, -Inf, NaN, 3.14159265358979, NA), .Names = c(\" 100\", \"-1e-13\", \" Inf\", \"-Inf\", \" NaN\", \"3.14\", \"  NA\")))"));  
do.call(`is.infinite`, argv);  
}, o=expected);  

