expected <- eval(parse(text="33333.3333333333"));               
test(id=0, code={               
argv <- eval(parse(text="list(1e+05, 3)"));               
do.call(`/`, argv);               
}, o=expected);               

