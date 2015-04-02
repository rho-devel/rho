expected <- eval(parse(text="1001"));  
test(id=0, code={  
argv <- eval(parse(text="list(1001)"));  
do.call(`ceiling`, argv);  
}, o=expected);  

