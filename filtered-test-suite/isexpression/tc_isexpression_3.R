expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE))"));              
do.call(`is.expression`, argv);              
}, o=expected);              

