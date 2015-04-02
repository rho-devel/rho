expected <- eval(parse(text="c(TRUE, NA, FALSE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(1L, NA, 0L))"));         
do.call(`as.logical`, argv);         
}, o=expected);         

