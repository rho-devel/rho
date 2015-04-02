expected <- eval(parse(text="1"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE)"));  
do.call(`cosh`, argv);  
}, o=expected);  

