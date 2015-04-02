expected <- eval(parse(text="c(NA, 26.04, 49.56, 65.04, 75.12, 84.96, 97.56, 115.8, NA)"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(NA, 6.12, 19.92, 29.64, 35.4, 39.72, 45.24, 52.32, 63.48), c(6.12, 19.92, 29.64, 35.4, 39.72, 45.24, 52.32, 63.48, NA))"));               
do.call(`+`, argv);               
}, o=expected);               

