expected <- eval(parse(text="c(TRUE, FALSE, TRUE, TRUE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(3.74165738677394, 0, 8.55235974119758, 1.96396101212393))"));         
do.call(`as.logical`, argv);         
}, o=expected);         

