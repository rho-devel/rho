expected <- eval(parse(text="FALSE"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1, 5.75, 10.5, 15.25, 20))"));  
do.call(`is.recursive`, argv);  
}, o=expected);  

