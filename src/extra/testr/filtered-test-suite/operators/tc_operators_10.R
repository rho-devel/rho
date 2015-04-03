expected <- eval(parse(text="1+1i"));               
test(id=0, code={               
argv <- eval(parse(text="list(1, 0+1i)"));               
do.call(`+`, argv);               
}, o=expected);               

