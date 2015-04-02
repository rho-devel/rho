expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(3.14159265358979)"));   
do.call(`is.nan`, argv);   
}, o=expected);   

