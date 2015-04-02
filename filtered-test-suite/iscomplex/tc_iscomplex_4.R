expected <- eval(parse(text="TRUE"));   
test(id=0, code={   
argv <- eval(parse(text="list(NA_complex_)"));   
do.call(`is.complex`, argv);   
}, o=expected);   

