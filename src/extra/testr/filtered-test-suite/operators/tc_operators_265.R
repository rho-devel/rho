expected <- eval(parse(text="TRUE"));   
test(id=0, code={   
argv <- eval(parse(text="list(3.001e+155, 0)"));   
do.call(`>`, argv);   
}, o=expected);   

