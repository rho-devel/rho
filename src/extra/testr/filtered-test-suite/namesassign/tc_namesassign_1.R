expected <- eval(parse(text="structure(list(happy = c(\"a\", \"b\", \"c\", \"d\"), sad = c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\")), .Names = c(\"happy\", \"sad\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(happy = c(\"a\", \"b\", \"c\", \"d\"), sad = c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\")), .Names = c(\"happy\", \"sad\")), value = c(\"happy\", \"sad\"))"));  
do.call(`names<-`, argv);  
}, o=expected);  

