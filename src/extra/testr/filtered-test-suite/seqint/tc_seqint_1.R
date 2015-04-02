expected <- eval(parse(text="16146:16149"));   
test(id=0, code={   
argv <- eval(parse(text="list(16146, by = 1, length.out = 4)"));   
do.call(`seq.int`, argv);   
}, o=expected);   

