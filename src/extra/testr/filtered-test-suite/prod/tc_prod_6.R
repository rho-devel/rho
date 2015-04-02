expected <- eval(parse(text="0"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(2, 0, 1, 2), .Dim = c(2L, 2L), .Dimnames = list(c(\"A\", \"B\"), c(\"A\", \"B\"))))"));          
do.call(`prod`, argv);          
}, o=expected);          

