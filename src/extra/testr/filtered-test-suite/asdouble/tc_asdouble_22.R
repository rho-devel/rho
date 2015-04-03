expected <- eval(parse(text="c(1, 0, 1)"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(TRUE, FALSE, TRUE))"));               
do.call(`as.double`, argv);               
}, o=expected);               

