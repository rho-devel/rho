expected <- eval(parse(text="list()"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(), NULL)"));  
do.call(`oldClass<-`, argv);  
}, o=expected);  

