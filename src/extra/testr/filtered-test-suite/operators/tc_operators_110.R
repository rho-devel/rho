expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, NA, TRUE, TRUE)"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, NA, 2L, 2L, 0L, NA, 1L, 1L), c(0, 0, 0, 0, 0, 0, 0, 0, 1, NA, 2, 2, 0, NA, 1, 1))"));            
do.call(`==`, argv);            
}, o=expected);            

