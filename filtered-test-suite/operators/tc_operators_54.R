expected <- eval(parse(text="0-1e-04i"));               
test(id=0, code={               
argv <- eval(parse(text="list(0+1e-04i)"));               
do.call(`-`, argv);               
}, o=expected);               

