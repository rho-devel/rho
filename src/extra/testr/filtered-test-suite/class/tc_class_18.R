expected <- eval(parse(text="\"array\""));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(numeric(0), .Dim = 0L))"));              
do.call(`class`, argv);              
}, o=expected);              

