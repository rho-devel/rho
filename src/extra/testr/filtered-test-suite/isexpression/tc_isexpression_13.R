expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(1.79769313486232e+308)"));              
do.call(`is.expression`, argv);              
}, o=expected);              

