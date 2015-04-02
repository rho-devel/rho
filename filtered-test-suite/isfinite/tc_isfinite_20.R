expected <- eval(parse(text="structure(c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE), .Names = c(\" 100\", \"-1e-13\", \" Inf\", \"-Inf\", \" NaN\", \"3.14\", \"  NA\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(100L, 0L, NA, NA, NA, 3L, NA), .Names = c(\" 100\", \"-1e-13\", \" Inf\", \"-Inf\", \" NaN\", \"3.14\", \"  NA\")))"));      
do.call(`is.finite`, argv);      
}, o=expected);      

