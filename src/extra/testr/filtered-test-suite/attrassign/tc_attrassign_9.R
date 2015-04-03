expected <- eval(parse(text="structure(1:3, .Names = c(\"a\", \"b\", \"c\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1:3, .Names = c(\"a\", \"b\", \"c\")), \"names\", value = list(\"a\", \"b\", \"c\"))"));  
do.call(`attr<-`, argv);  
}, o=expected);  

