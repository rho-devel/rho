expected <- eval(parse(text="c(6, 12, 17)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(6, 6, 5))"));  
do.call(`cumsum`, argv);  
}, o=expected);  

