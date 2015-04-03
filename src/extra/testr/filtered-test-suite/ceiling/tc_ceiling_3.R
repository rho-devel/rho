expected <- eval(parse(text="c(1, 5, 8, 12, 15)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1, 4.5, 8, 11.5, 15))"));  
do.call(`ceiling`, argv);  
}, o=expected);  

