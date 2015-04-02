expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1:24, .Dim = 2:4))"));   
do.call(`is.complex`, argv);   
}, o=expected);   

