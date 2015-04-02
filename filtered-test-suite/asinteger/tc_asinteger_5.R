expected <- eval(parse(text="200000L"));              
test(id=0, code={              
argv <- eval(parse(text="list(2e+05)"));              
do.call(`as.integer`, argv);              
}, o=expected);              

