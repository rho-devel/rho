expected <- eval(parse(text="structure(c(4.14159265358979+1i, 5.14159265358979+2i, 4.3415926535898+10i, 5.5415926535898+20i), .Dim = c(2L, 2L), .Dimnames = list(c(\"x\", \"\"), c(\"a\", \"b\")))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(1+1i, 2+2i, 1.2+10i, 2.4+20i), .Dim = c(2L, 2L), .Dimnames = list(c(\"x\", \"\"), c(\"a\", \"b\"))), 3.14159265358979)"));               
do.call(`+`, argv);               
}, o=expected);               

