expected <- eval(parse(text="4L"));   
test(id=0, code={   
argv <- eval(parse(text="list(4, 4L)"));   
do.call(`seq.int`, argv);   
}, o=expected);   

