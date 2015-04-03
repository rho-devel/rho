expected <- eval(parse(text="FALSE"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(NA, 9, 3, 3))"));  
do.call(`is.integer`, argv);  
}, o=expected);  

