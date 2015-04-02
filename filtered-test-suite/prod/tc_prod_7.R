expected <- eval(parse(text="1"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE), .Dim = c(2L, 2L), .Dimnames = list(c(\"A\", \"B\"), c(\"A\", \"B\"))))"));          
do.call(`prod`, argv);          
}, o=expected);          

