expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(2L, 1)"));   
do.call(`==`, argv);   
}, o=expected);   

