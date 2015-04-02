expected <- eval(parse(text="list()"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(), value = 0L)"));  
do.call(`length<-`, argv);  
}, o=expected);  

