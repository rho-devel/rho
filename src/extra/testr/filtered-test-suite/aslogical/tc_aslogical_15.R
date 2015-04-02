expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(1, 2, 3, 4, 5, NA, NA, 2, 3, 4, 5, 6))"));         
do.call(`as.logical`, argv);         
}, o=expected);         

