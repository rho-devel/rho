expected <- eval(parse(text="NA_complex_"));   
test(id=0, code={   
argv <- eval(parse(text="list(\" \")"));   
do.call(`as.complex`, argv);   
}, o=expected);   

