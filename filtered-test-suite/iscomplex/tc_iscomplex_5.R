expected <- eval(parse(text="TRUE"));   
test(id=0, code={   
argv <- eval(parse(text="list(1.3+0i)"));   
do.call(`is.complex`, argv);   
}, o=expected);   

