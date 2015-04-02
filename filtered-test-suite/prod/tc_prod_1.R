expected <- eval(parse(text="9"));   
test(id=0, code={   
argv <- eval(parse(text="list(9L)"));   
do.call(`prod`, argv);   
}, o=expected);   

