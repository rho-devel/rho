expected <- eval(parse(text="c(1, 2, 3, NA, -1, 0, 1, NA)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(c(1L, 2L, 3L, NA), c(-1, 0, 1, NA))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

