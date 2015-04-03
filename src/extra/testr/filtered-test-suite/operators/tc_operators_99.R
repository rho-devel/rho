expected <- eval(parse(text="TRUE"));             
test(id=0, code={             
argv <- eval(parse(text="list(150000, 3e+09)"));             
do.call(`<=`, argv);             
}, o=expected);             

