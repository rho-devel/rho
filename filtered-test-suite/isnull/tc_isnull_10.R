expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(complex(0))"));   
do.call(`is.null`, argv);   
}, o=expected);   

