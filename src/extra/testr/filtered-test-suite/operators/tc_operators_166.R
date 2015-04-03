expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Names = c(\" 100\", \"-1e-13\", \" Inf\", \"-Inf\", \" NaN\", \"3.14\", \"  NA\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE), .Names = c(\" 100\", \"-1e-13\", \" Inf\", \"-Inf\", \" NaN\", \"3.14\", \"  NA\")), structure(c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE), .Names = c(\" 100\", \"-1e-13\", \" Inf\", \"-Inf\", \" NaN\", \"3.14\", \"  NA\")))"));  
do.call(`!=`, argv);  
}, o=expected);  

