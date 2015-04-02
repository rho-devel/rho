expected <- eval(parse(text="\"logical\""));              
test(id=0, code={              
argv <- eval(parse(text="list(c(FALSE, FALSE, FALSE, NA, NA, TRUE, TRUE, TRUE, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))"));              
do.call(`class`, argv);              
}, o=expected);              

