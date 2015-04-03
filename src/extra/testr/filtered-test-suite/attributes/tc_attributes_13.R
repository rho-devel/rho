expected <- eval(parse(text="NULL"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))"));               
do.call(`attributes`, argv);               
}, o=expected);               

