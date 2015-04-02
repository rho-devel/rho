expected <- eval(parse(text="41L"));               
test(id=0, code={               
argv <- eval(parse(text="list(41L, 0L)"));               
do.call(`-`, argv);               
}, o=expected);               

