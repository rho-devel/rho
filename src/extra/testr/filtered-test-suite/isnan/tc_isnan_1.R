expected <- eval(parse(text="FALSE"));  
test(id=0, code={  
argv <- eval(parse(text="list(NA)"));  
do.call(`is.nan`, argv);  
}, o=expected);  

