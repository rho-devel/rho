expected <- eval(parse(text="NA_complex_"));   
test(id=0, code={   
argv <- eval(parse(text="list(NA_complex_)"));   
do.call(`Conj`, argv);   
}, o=expected);   

