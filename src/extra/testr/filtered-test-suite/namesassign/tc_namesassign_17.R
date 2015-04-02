expected <- eval(parse(text="structure(c(1+1i, 1.2+10i), .Names = c(\"a\", \"b\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(1+1i, 1.2+10i), .Names = c(\"a\", \"b\")), value = c(\"a\", \"b\"))"));       
do.call(`names<-`, argv);       
}, o=expected);       

