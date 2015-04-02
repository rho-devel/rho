expected <- eval(parse(text="c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(NA, 5, 9, 1, 2, 5, 6, 7, 8, 3, 8))"));              
do.call(`is.finite`, argv);              
}, o=expected);              

