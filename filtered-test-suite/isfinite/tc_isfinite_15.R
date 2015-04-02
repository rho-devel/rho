expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(0.0099, 0.099, 0.99, 9.9, 99, 990, 9900, 99000, 990000, 9900000, 9.9e+07, 9.9e+08, 9.9e+09, 9.9e+10))"));              
do.call(`is.finite`, argv);              
}, o=expected);              

