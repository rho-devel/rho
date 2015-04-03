expected <- eval(parse(text="c(NA, 0.0021)"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(NA, \"0.0021\"))"));               
do.call(`as.double`, argv);               
}, o=expected);               

