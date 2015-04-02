expected <- eval(parse(text="c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE), c(TRUE, TRUE, TRUE, NA, FALSE, FALSE, TRUE, NA))"));       
do.call(`&`, argv);       
}, o=expected);       

