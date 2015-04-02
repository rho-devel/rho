expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(NA, 0L))"));  
do.call(`is.integer`, argv);  
}, o=expected);  

