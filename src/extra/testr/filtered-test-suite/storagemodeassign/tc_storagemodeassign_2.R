expected <- eval(parse(text="3.14159265358979+0i"));  
test(id=0, code={  
argv <- eval(parse(text="list(3.14159265358979, value = \"complex\")"));  
do.call(`storage.mode<-`, argv);  
}, o=expected);  

