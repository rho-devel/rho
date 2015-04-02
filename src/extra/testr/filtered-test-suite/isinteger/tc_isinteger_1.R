expected <- eval(parse(text="FALSE"));  
test(id=0, code={  
argv <- eval(parse(text="list(2.74035772634541)"));  
do.call(`is.integer`, argv);  
}, o=expected);  

