expected <- eval(parse(text="numeric(0)"));              
test(id=0, code={              
argv <- eval(parse(text="list(integer(0), 1:3)"));              
do.call(`^`, argv);              
}, o=expected);              

