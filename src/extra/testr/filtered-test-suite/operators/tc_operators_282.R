expected <- eval(parse(text="0"));              
test(id=0, code={              
argv <- eval(parse(text="list(0.9, Inf)"));              
do.call(`^`, argv);              
}, o=expected);              

