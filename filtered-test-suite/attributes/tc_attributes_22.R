expected <- eval(parse(text="NULL"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(1000, 1e+07, 1))"));               
do.call(`attributes`, argv);               
}, o=expected);               

