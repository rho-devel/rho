expected <- eval(parse(text="TRUE"));   
test(id=0, code={   
argv <- eval(parse(text="list(complex(0))"));   
do.call(`is.complex`, argv);   
}, o=expected);   

