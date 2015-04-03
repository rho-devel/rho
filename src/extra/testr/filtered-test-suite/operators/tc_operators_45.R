expected <- eval(parse(text="14"));               
test(id=0, code={               
argv <- eval(parse(text="list(38, 24L)"));               
do.call(`-`, argv);               
}, o=expected);               

