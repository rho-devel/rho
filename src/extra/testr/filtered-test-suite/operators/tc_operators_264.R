expected <- eval(parse(text="TRUE"));   
test(id=0, code={   
argv <- eval(parse(text="list(75.1931882101063, 0)"));   
do.call(`>`, argv);   
}, o=expected);   

