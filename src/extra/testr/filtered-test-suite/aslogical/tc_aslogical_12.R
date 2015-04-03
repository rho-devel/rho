expected <- eval(parse(text="c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))"));         
do.call(`as.logical`, argv);         
}, o=expected);         

