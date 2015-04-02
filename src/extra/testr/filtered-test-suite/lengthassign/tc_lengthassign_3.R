expected <- eval(parse(text="list(list(2, 2, 6))"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(list(2, 2, 6), list(1, 3, 9), list(1, 3, -1)), value = 1)"));  
do.call(`length<-`, argv);  
}, o=expected);  

