expected <- eval(parse(text="c(\"A\", \"B\", NA, NA, NA)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"A\", \"B\"), value = 5)"));  
do.call(`length<-`, argv);  
}, o=expected);  

