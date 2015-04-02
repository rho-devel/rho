expected <- eval(parse(text="\"José Pinheiro [aut] (S version)\""));   
test(id=0, code={   
argv <- eval(parse(text="list(\"José Pinheiro [aut] (S version)\")"));   
do.call(`enc2native`, argv);   
}, o=expected);   

