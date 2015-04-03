expected <- eval(parse(text="NaN"));             
test(id=0, code={             
argv <- eval(parse(text="list(Inf)"));             
do.call(`sin`, argv);             
}, o=expected);             

