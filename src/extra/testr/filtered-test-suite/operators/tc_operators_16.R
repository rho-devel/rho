expected <- eval(parse(text="1.678932e-305+0i"));               
test(id=0, code={               
argv <- eval(parse(text="list(1.678932e-305, 0+0i)"));               
do.call(`+`, argv);               
}, o=expected);               

