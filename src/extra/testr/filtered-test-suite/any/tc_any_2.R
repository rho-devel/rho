expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE), .Dim = 3L, .Dimnames = list(c(\"A\", \"B\", \"C\"))))"));             
do.call(`any`, argv);             
}, o=expected);             

