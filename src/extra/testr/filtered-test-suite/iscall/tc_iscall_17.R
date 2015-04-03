expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(1.1+0i, NA, 3+0i))"));              
do.call(`is.call`, argv);              
}, o=expected);              

