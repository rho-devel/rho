expected <- eval(parse(text="7L"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(c(3.14159265358979e-10, 0.0314159265358979, 3.14159265358979, 31.4159265358979, 314.159265358979, 314159265.358979, 3.14159265358979e+20))"));                   
do.call(`length`, argv);                   
}, o=expected);                   

