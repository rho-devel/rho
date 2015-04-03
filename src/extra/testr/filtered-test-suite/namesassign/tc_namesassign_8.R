expected <- eval(parse(text="structure(c(\"variable1\", \"variable2\"), .Names = c(\"variable1\", \"variable2\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(\"variable1\", \"variable2\"), .Names = c(\"variable1\", \"variable2\")), value = c(\"variable1\", \"variable2\"))"));       
do.call(`names<-`, argv);       
}, o=expected);       

