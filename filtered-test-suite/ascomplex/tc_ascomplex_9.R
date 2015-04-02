expected <- eval(parse(text="1+0i"));   
test(id=0, code={   
argv <- eval(parse(text="list(1L)"));   
do.call(`as.complex`, argv);   
}, o=expected);   

