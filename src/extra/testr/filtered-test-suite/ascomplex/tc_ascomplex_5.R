expected <- eval(parse(text="1.3+0i"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"1.3\")"));   
do.call(`as.complex`, argv);   
}, o=expected);   

