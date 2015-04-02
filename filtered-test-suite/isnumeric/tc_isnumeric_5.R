expected <- eval(parse(text="TRUE"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(16.4, 11.4, 7.8, 14, 10.9, 16.8, 16.6, 5.9, 21))"));               
do.call(`is.numeric`, argv);               
}, o=expected);               

