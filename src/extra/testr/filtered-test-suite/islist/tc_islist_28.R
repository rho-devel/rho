expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1, .Dim = 1L))"));       
do.call(`is.list`, argv);       
}, o=expected);       

