expected <- eval(parse(text="list()"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(list(2, 2, 6), list(2, 2, 0)), value = 0)"));  
do.call(`length<-`, argv);  
}, o=expected);  

