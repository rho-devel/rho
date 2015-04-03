expected <- eval(parse(text="c(0, 1, 2)"));   
test(id=0, code={   
argv <- eval(parse(text="list(0, length.out = 3L)"));   
do.call(`seq.int`, argv);   
}, o=expected);   

