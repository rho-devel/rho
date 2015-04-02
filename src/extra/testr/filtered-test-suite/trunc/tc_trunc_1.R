expected <- eval(parse(text="8"));  
test(id=0, code={  
argv <- eval(parse(text="list(8.5)"));  
do.call(`trunc`, argv);  
}, o=expected);  

