expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(3.97376540705816e-12)"));              
do.call(`is.expression`, argv);              
}, o=expected);              

