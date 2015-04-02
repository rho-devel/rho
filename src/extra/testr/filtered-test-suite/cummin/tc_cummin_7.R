expected <- eval(parse(text="numeric(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(NULL)"));  
do.call(`cummin`, argv);  
}, o=expected);  

