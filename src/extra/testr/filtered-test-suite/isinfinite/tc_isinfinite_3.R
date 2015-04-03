expected <- eval(parse(text="structure(logical(0), .Dim = c(0L, 0L))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(logical(0), .Dim = c(0L, 0L)))"));       
do.call(`is.infinite`, argv);       
}, o=expected);       

