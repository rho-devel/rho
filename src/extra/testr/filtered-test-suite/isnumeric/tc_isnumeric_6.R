expected <- eval(parse(text="TRUE"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(0, 0, 0, 1, 0, 0, 0, 0, 1, 0, NA, NA, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, NA, 1, 0, 1, 0, NA, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, NA, 0, 1, 0, 0, 0, 0, NA, 1, 1))"));               
do.call(`is.numeric`, argv);               
}, o=expected);               

