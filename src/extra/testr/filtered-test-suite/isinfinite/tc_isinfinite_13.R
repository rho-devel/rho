expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = 2:4)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1:24, .Dim = 2:4))"));       
do.call(`is.infinite`, argv);       
}, o=expected);       

