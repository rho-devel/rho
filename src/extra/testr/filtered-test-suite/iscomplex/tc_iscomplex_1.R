expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(integer(0))"));   
do.call(`is.complex`, argv);   
}, o=expected);   

