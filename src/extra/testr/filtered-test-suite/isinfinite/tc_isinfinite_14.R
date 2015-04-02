expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE), .Dim = c(2L, 2L), .Dimnames = list(c(\"A\", \"B\"), c(\"A\", \"B\")))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(1, 0.5, 0.5, 1), .Dim = c(2L, 2L), .Dimnames = list(c(\"A\", \"B\"), c(\"A\", \"B\"))))"));       
do.call(`is.infinite`, argv);       
}, o=expected);       

