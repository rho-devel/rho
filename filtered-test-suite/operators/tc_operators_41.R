expected <- eval(parse(text="-1.15"));               
test(id=0, code={               
argv <- eval(parse(text="list(1.15)"));               
do.call(`-`, argv);               
}, o=expected);               

