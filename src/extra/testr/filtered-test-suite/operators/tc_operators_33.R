expected <- eval(parse(text="c(31.2, 16.8, 9.3, 19.2, 65.1, 20.8, 21.6, 11.8, 15.9, 49.8, 42.5)"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(20.8, 11.2, 6.2, 12.8, 43.4), c(10.4, 5.6, 3.1, 6.4, 21.7, 0, 10.4, 5.6, 3.1, 6.4, 21.7))"));               
do.call(`+`, argv);               
}, o=expected);               

